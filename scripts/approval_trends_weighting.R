# libraries ----
library(tidyverse)
library(lubridate)
library(furrr)
library(riekelib)
library(patchwork)
library(shadowtext)

# cleanup ----
rm(list = ls())

# themes ----
source("https://raw.githubusercontent.com/markjrieke/thedatadiary/main/dd_theme_elements/dd_theme_elements.R")
theme_set(theme_minimal(base_family = "Roboto Slab"))

# setup parallel processing (Windows) ----
num_cores <- parallel::detectCores()
clusters <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(clusters)

# setup path ----
path <- "data/polls/src/fte/"

# load data ----
approval_polls <- read_csv(paste0(path, "president_approval_polls_historical.csv")) 
approval_trends <- read_csv(paste0(path, "approval_topline.csv"))

# pre function wrangle ----

# parse down frames to only useful cols; light reformatting
approval_polls <- 
  approval_polls %>%
  select(display_name, sample_size, population_full,
         methodology, end_date, yes, no) %>%
  mutate(end_date = mdy(end_date),
         methodology = replace_na(methodology, "Unknown"),
         yes = yes/100,
         no = no/100) %>%
  drop_na() %>%
  rename(pollster = display_name)

# separate approval & disapproval trends
approval_trends <- 
  approval_trends %>%
  filter(subgroup == "All polls") %>%
  select(modeldate:disapprove_lo) %>%
  mutate(modeldate = mdy(modeldate),
         across(approve_estimate:disapprove_lo, ~.x/100)) %>%
  rename(date = modeldate)

disapproval_trends <-
  approval_trends %>%
  select(date, disapprove_estimate:disapprove_lo)

approval_trends <-
  approval_trends %>%
  select(date, approve_estimate:approve_lo)

# create a list of pollsters who make up at least 1% of the approval polls
pollsters <- 
  approval_polls %>%
  percent(pollster) %>%
  arrange(desc(pct)) %>%
  filter(pct >= 0.01) %>%
  pull(pollster)

# create a list of methodologies that have been used in at least 1% of approval polls
methods <- 
  approval_polls %>%
  percent(methodology) %>%
  filter(pct >= 0.01) %>%
  arrange(desc(pct)) %>%
  pull(methodology)

# replace pollsters/methodologies that don't occur often with "Other"
approval_polls <- 
  approval_polls %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  drop_na()

# create vectors for the approval timeframe
begin <- rep(rep(ymd("2017-01-22"), 1459), 5)
final <- rep(seq(ymd("2017-01-23"), ymd("2021-01-20"), "days"), 5)

#################### GENERIC APPROVAL AVERAGE FUNCTION ####################

#' Return the weighted polling average of polls conducted from `begin_date` to `end_date`
#' 
#' @param .data dataframe or tibble of approval polls
#' @param begin_date earliest date to include polls, by polling period end_date.
#' @param final_date last date to include polls, by polling period end_date.
#' @param pollster_weight tibble of pollster weights and offsets
#' @param sample_weight sample size weight (relative to a sample size of 1000)
#' @param population_weight tibble of weights by survey population
#' @param method_weight tibble of weights by survey methodology
#' @param date_weight weight for exponential decay function
#' @param answer choose whether to calculate approval (yes) or disapproval (no)
approval_average <- function(.data,
                             begin_date,
                             final_date, 
                             pollster_weight,
                             sample_weight,
                             population_weight,
                             method_weight,
                             date_weight,
                             answer,
                             downweight = 1) {
  
  .data %>%
    
    # filter to just the relevant dates
    filter(end_date <= final_date,
           end_date >= begin_date) %>%
    
    # apply pollster weights and offsets
    left_join(pollster_weight, by = "pollster") %>%
    mutate(answer = {{answer}} + pollster_offset,
           answer_votes = round({{answer}} * sample_size),
           not_answer_votes = round((1-{{answer}}) * sample_size)) %>%
    select(-pollster_offset) %>%
    
    # apply sample size weight
    mutate(sample_weight = log10(sample_size) * sample_weight) %>%
    
    # apply population weight
    left_join(population_weight, by = "population_full") %>%
    
    # apply methodology weight
    left_join(method_weight, by = "methodology") %>%
    
    # apply date weight
    mutate(days_diff = as.numeric(final_date - end_date) + 1,
           weeks_diff = days_diff/7,
           date_weight = date_weight ^ weeks_diff) %>%
    select(-days_diff, -weeks_diff) %>%
    
    # create individual poll weights
    mutate(alpha = answer_votes * pollster_weight * sample_weight * population_weight * method_weight * date_weight * downweight,
           beta = not_answer_votes * pollster_weight * sample_weight * population_weight * method_weight * date_weight * downweight) %>%
    
    # summarise with a weak uniform prior
    summarise(alpha = sum(alpha) + 1,
              beta = sum(beta) + 1) %>%
    mutate(answer = alpha/(alpha + beta),
           date = final_date) %>%
    beta_interval(alpha, beta) %>%
    select(date, answer, ci_lower, ci_upper)
  
}

#################### UTIL FUNCTIONS ####################

# initialize weights and offsets
initialize_weights <- function() {
  
  bind_rows(
    
    # pollster weights
    tibble(variable = c(pollsters, "Other Pollster"), 
           weight = 1,
           next_lower = 0,
           next_upper = 1),
    
    # pollster offsets
    tibble(variable = c(paste(pollsters, "Offset"), "Other Pollster Offset"),
           weight = 0,
           next_lower = -0.1,
           next_upper = 0.1),
    
    # date weights
    tibble(variable = "date_weight",
           weight = 1,
           next_lower = 0,
           next_upper = 1),
    
    # sample size weights
    tibble(variable = "sample_size",
           weight = 1,
           next_lower = 0,
           next_upper = 1),
    
    # population weights
    tibble(variable = approval_polls %>% distinct(population_full) %>% pull(population_full),
           weight = 1,
           next_lower = 0,
           next_upper = 1),
    
    # methodology weights
    tibble(variable = c(methods, "Other Method"),
           weight = 1,
           next_lower = 0,
           next_upper = 1)
  ) %>%
    
    # initialize with not final as the search suggestion for each variable
    bind_cols(search_suggestion = "not final")
  
}

# construct a tibble for pollster weights and offsets
pull_pollster_weights <- function(.data) {
  
  .data %>%
    filter(str_detect(variable, " Offset")) %>%
    select(variable, weight) %>%
    rename(offset = weight) %>%
    mutate(variable = str_remove(variable, " Offset")) %>%
    left_join(.data, by = "variable") %>%
    select(-starts_with("next")) %>%
    rename(pollster = variable,
           pollster_offset = offset,
           pollster_weight = weight)
  
} 

# pull sample_size weight
pull_sample_weight <- function(.data) {
  
  .data %>%
    filter(variable == "sample_size") %>%
    pull(weight)
  
}

# construct a tibble for weights by survey population
pull_population_weights <- function(.data) {
  
  .data %>%
    filter(variable %in% c("rv", "lv", "a", "v")) %>%
    select(variable, weight) %>%
    rename(population_full = variable,
           population_weight = weight)
  
} 

# construct a tibble for weights by survey methodology
pull_methodology_weights <- function(.data) {
  
  .data %>%
    filter(variable %in% c(methods, "Other Method")) %>%
    select(variable, weight) %>%
    rename(methodology = variable,
           method_weight = weight)
  
} 

# pull date_weight
pull_date_weight <- function(.data) {
  
  .data %>%
    filter(variable == "date_weight") %>%
    pull(weight)
  
} 

# check if the search suggestion is final or ought to continue
check_suggestion <- function(.data, variable_name) {
  
  .data %>%
    filter(variable == variable_name) %>%
    pull(search_suggestion)
  
}

# util function to create a temporary tibble replacing the current weight with a "try" weight
pull_try_weight <- function(.data, variable_name, new_weight, type) {
  
  # create a new weight table to pass to one of the pull functions
  new_weight_tibble <- 
    .data %>%
    filter(variable != variable_name) %>%
    bind_rows(tibble(variable = variable_name,
                     weight = new_weight,
                     next_lower = 0,
                     next_upper = 0))
  
  # pass new weight table to one of the pull functions based on param type 
  if (type == "pollster") {
    
    pulled_tibble <- new_weight_tibble %>% pull_pollster_weights()
    
  } else if (type == "population") {
    
    pulled_tibble <- new_weight_tibble %>% pull_population_weights()
    
  } else {
    
    pulled_tibble <- new_weight_tibble %>% pull_methodology_weights()
    
  }
  
  return(pulled_tibble)
  
}

# function to pull the next lower or the next upper bound
pull_bound <- function(.data, variable_name, type) {
  
  if (type == "lower") {
    
    .data %>%
      filter(variable == variable_name) %>%
      pull(next_lower)
    
  } else {
    
    .data %>%
      filter(variable == variable_name) %>%
      pull(next_upper)
    
  }
  
}

# create a sequence of new weights to try
sequence_weights <- function(lower_bound, upper_bound) {
  
  try_weight <- seq(lower_bound, upper_bound, length.out = 5)
  weights <- try_weight %>% rep(1459) %>% arrange_vector()
  
  return(weights)
  
}

# create a vector of weights to be passed to try_list
vectorize_weights <- function(.data, variable_name) {
  
  # generate lower & upper bounds
  lower_bound <-
    .data %>%
    pull_bound(variable_name, "lower")
  
  upper_bound <- 
    .data %>%
    pull_bound(variable_name, "upper")
  
  # create a sequence of new weights to try
  weights <- sequence_weights(lower_bound, upper_bound)
  
  return(weights)
  
}

# create a list of inputs to map to the approval/disapproval tracker
create_try_list <- function(weights) {
  
  list(weights = weights,
       begin = begin,
       final = final)
  
}

# summarise results & recommend new bounds 
summarise_weights <- function(.data, metric, type) {
  
  # create tibble of rmse for each weight evaluated
  weight_metrics <-
    .data %>%
    select(-starts_with("ci"))
  
  # join fte trendline
  if (type == "approval") {
    
    weight_metrics <-
      weight_metrics %>%
      left_join(approval_trends, by = "date")

  } else {
    
    weight_metrics <-
      weight_metrics %>%
      left_join(disapproval_trends, by = "date")
    
  }
  
  # calculate each weight's rmse
  weight_metrics <-
    weight_metrics %>%
    rename(truth = ends_with("estimate")) %>%
    group_by(weight) %>%
    nest() %>%
    mutate(rmse = map(data, yardstick::rmse, truth = truth, estimate = answer)) %>%
    unnest(rmse) %>%
    ungroup() %>%
    rowid_to_column() %>%
    select(rowid, weight, .estimate) %>%
    rename(rmse = .estimate)
  
  # find weight that gives the smallest rmse
  best_weight <- 
    weight_metrics %>%
    filter(rmse == min(rmse)) %>%
    filter(rowid == min(rowid)) %>%
    pull(weight)
  
  # pull the smallest rmse for tracking
  best_rmse <- 
    weight_metrics %>%
    filter(rmse == min(rmse)) %>%
    filter(rowid == min(rowid)) %>%
    pull(rmse)
  
  # pull the row index that gives the best rmse
  best_index <-
    weight_metrics %>%
    filter(weight == best_weight) %>%
    pull(rowid)
  
  # get the step between each weight
  delta <-
    weight_metrics %>%
    mutate(delta = weight - lag(weight)) %>%
    drop_na() %>%
    filter(rowid == 5) %>%
    pull(delta)
  
  # assign next_upper & next_lower
  if (best_index == 5) {
    
    next_lower <- weight_metrics %>% filter(rowid == 3) %>% pull(weight)
    next_upper <- weight_metrics %>% filter(rowid == 5) %>% pull(weight) + (2 * delta)
    
  } else if (best_index == 1) {
    
    if (best_weight == 0 & str_detect(metric, "Offset") == FALSE) {
      
      next_lower <- 0
      next_upper <- weight_metrics %>% filter(rowid == 2) %>% pull(weight)
      
    } else {
      
      next_lower <- weight_metrics %>% filter(rowid == 1) %>% pull(weight) - (2 * delta)
      next_upper <- weight_metrics %>% filter(rowid == 3) %>% pull(weight)
      
    }
    
  } else {
    
    next_lower <- weight_metrics %>% filter(rowid == best_index - 1) %>% pull(weight)
    next_upper <- weight_metrics %>% filter(rowid == best_index + 1) %>% pull(weight)
    
  }
  
  # check for % difference between best and worst rmse
  worst_rmse <- 
    weight_metrics %>%
    filter(rmse == max(rmse)) %>%
    filter(rowid == max(rowid)) %>%
    pull(rmse)
  
  pct_diff <- abs(worst_rmse - best_rmse)/mean(c(best_rmse, worst_rmse))
  
  # for pollster weights, update weight table to rowid == 2 if best rmse = 0
  # this will ensure that the pollster's offset actually gets evaluated!
  if (best_weight == 0 & metric %in% c(pollsters, "Other Pollster")) {
    
    best_weight <-
      weight_metrics %>%
      filter(rowid == 2) %>%
      pull(weight)
    
  }
  
  # note whether or not to continue to search for better weights, based of difference threshold
  if (pct_diff < 0.01) {
    
    search_suggestion <- "final"
    
  } else {
    
    search_suggestion <- "not final"
    
  }
  
  # set offset to 0 if < 1% difference 
  # this'll avoid having a pollster offset of -10% when it's not any different from an offset of 0%
  if (pct_diff < 0.01 & str_detect(metric, "Offset") == TRUE) {
    
    zero_len <- 
      weight_metrics %>%
      filter(weight == 0) %>%
      pull(weight) %>%
      length()
    
    if (zero_len > 0) {
      
      best_weight <- 0
      
    }
      
  }
  
  # summarise in 1-row tibble
  weight_summary <-
    tibble(metric = metric,
           weight = best_weight,
           rmse = best_rmse,
           next_lower = next_lower,
           next_upper = next_upper,
           pct_diff = pct_diff,
           search_suggestion = search_suggestion)
  
  return(weight_summary)
  
}

# add metrics to running list of rmse
update_rmse_tracker <- function(.data, type) {
  
  # <<- interacts with the global object
  if (type == "approval") {
    
    approval_rmse_tracker <<-
      approval_rmse_tracker %>%
      
      # pull current index
      filter(index == max(index)) %>%
      select(index) %>%
      
      # increase index
      mutate(index = index + 1) %>%
      
      # add index to data
      bind_cols(.data) %>%
      
      # bind back to rmse_tracker
      bind_rows(approval_rmse_tracker) %>%
      arrange(index)
    
  } else {
    
    disapproval_rmse_tracker <<-
      disapproval_rmse_tracker %>%
      
      # pull current index
      filter(index == max(index)) %>%
      select(index) %>%
      
      # increase index
      mutate(index = index + 1) %>%
      
      # add index to data
      bind_cols(.data) %>%
      
      # bind back to rmse_tracker
      bind_rows(disapproval_rmse_tracker) %>%
      arrange(index)
    
  }
  
}

# function to update the variable weight table
update_weight_table <- function(.data, variable_name, type) {
  
  # <<- interact with global environment
  if (type == "approval") {
    
    approval_weights <<-
      approval_weights %>%
      filter(variable != variable_name) %>%
      
      # reformat & bind .data with new weights/suggestions
      bind_rows(.data %>% select(-rmse, -pct_diff) %>% rename(variable = metric))
    
  } else {
    
    disapproval_weights <<-
      disapproval_weights %>%
      filter(variable != variable_name) %>%
      
      # reformat & bind .data with new weights/suggestion
      bind_rows(.data %>% select(-rmse, -pct_diff) %>% rename(variable = metric)) 
    
  }
  
}

update_tables <- function(.data, variable_name, type) {
  
  # summarise results based on best rmse
  weight_summary <-
    .data %>%
    summarise_weights(variable_name, type)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker(type)
  
  # update weight table
  weight_summary %>% update_weight_table(variable_name, type)
  
}

# assign weight source based on type
weight_assignment <- function(type) {
  
  if (type == "approval") {
    
    approval_weights
    
  } else {
    
    disapproval_weights
    
  }
  
}

#################### WEIGHT UPDATE FUNCTIONS ####################

# function to update the weight for exponential decay by date 
update_date_weight <- function(type) {
  
  # set fn-level weight tibble from type
  variable_weights <- weight_assignment(type)
  
  # do not evaluate if weight is final
  if (variable_weights %>% check_suggestion("date_weight") == "final") {
    
    message("date_weight marked as final and will not be updated.")
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- variable_weights %>% vectorize_weights("date_weight")
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic approval fn
    weight_map <-
      try_list %>%
      future_pmap_dfr(~approval_average(approval_polls,
                                        ..2,
                                        ..3,
                                        pull_pollster_weights(variable_weights),
                                        pull_sample_weight(variable_weights),
                                        pull_population_weights(variable_weights),
                                        pull_methodology_weights(variable_weights),
                                        ..1,
                                        if (type == "approval") yes else no)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables("date_weight", type)
    
  }
  
}

# function to update the weight based on sample size 
update_sample_weight <- function(type) {
  
  # set fn-level weight tibble from type
  variable_weights <- weight_assignment(type)
  
  # do not evaluate if weight is final
  if (variable_weights %>% check_suggestion("sample_size") == "final") {
    
    message("sample_size marked as final and will not be updated.")
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- variable_weights %>% vectorize_weights("sample_size")
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic approval fn
    weight_map <-
      try_list %>%
      future_pmap_dfr(~approval_average(approval_polls,
                                        ..2,
                                        ..3,
                                        pull_pollster_weights(variable_weights),
                                        ..1,
                                        pull_population_weights(variable_weights),
                                        pull_methodology_weights(variable_weights),
                                        pull_date_weight(variable_weights),
                                        if (type == "approval") yes else no)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables("sample_size", type)
    
  }
  
}

# function to update the weight based on individual pollster 
update_pollster_weight <- function(pollster, type) {
  
  # set fn-level weight tibble from type
  variable_weights <- weight_assignment(type)
  
  # do not evaluate if weight is final
  if (variable_weights %>% check_suggestion(pollster) == "final") {
    
    message(paste(pollster, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- variable_weights %>% vectorize_weights(pollster)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic approval fn
    weight_map <-
      try_list %>%
      future_pmap_dfr(~approval_average(approval_polls,
                                        ..2,
                                        ..3,
                                        pull_try_weight(variable_weights, pollster, ..1, "pollster"),
                                        pull_sample_weight(variable_weights),
                                        pull_population_weights(variable_weights),
                                        pull_methodology_weights(variable_weights),
                                        pull_date_weight(variable_weights),
                                        if (type == "approval") yes else no)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(pollster, type)
    
  }
  
}

# function to update a pollster's offset 
update_pollster_offset <- function(pollster, type) {
  
  # create offset variable
  offset <- paste(pollster, "Offset")
  
  # set fn-level weight tibble from type
  variable_weights <- weight_assignment(type)
  
  # do not evaluate if weight is final
  if (variable_weights %>% check_suggestion(offset) == "final") {
    
    message(paste(offset, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- variable_weights %>% vectorize_weights(offset)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic approval fn
    weight_map <-
      try_list %>%
      future_pmap_dfr(~approval_average(approval_polls,
                                        ..2,
                                        ..3,
                                        pull_try_weight(variable_weights, offset, ..1, "pollster"),
                                        pull_sample_weight(variable_weights),
                                        pull_population_weights(variable_weights),
                                        pull_methodology_weights(variable_weights),
                                        pull_date_weight(variable_weights),
                                        if (type == "approval") yes else no)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(offset, type)
    
  }
  
}

# function to update weight based on population type
update_population_weight <- function(population, type) {
  
  # set fn-level weight tibble from type
  variable_weights <- weight_assignment(type)
  
  # do not evaluate if weight is final
  if (variable_weights %>% check_suggestion(population) == "final") {
    
    message(paste(population, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- variable_weights %>% vectorize_weights(population)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic approval fn
    weight_map <-
      try_list %>%
      future_pmap_dfr(~approval_average(approval_polls,
                                        ..2,
                                        ..3,
                                        pull_pollster_weights(variable_weights),
                                        pull_sample_weight(variable_weights),
                                        pull_try_weight(variable_weights, population, ..1, "population"),
                                        pull_methodology_weights(variable_weights),
                                        pull_date_weight(variable_weights),
                                        if (type == "approval") yes else no)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(population, type)
    
  }
  
}

# function to update weight based on methodology
update_methodology_weight <- function(methodology, type) {
  
  # set fn-level weight tibble from type
  variable_weights <- weight_assignment(type)
  
  # do not evaluate if weight is final
  if (variable_weights %>% check_suggestion(methodology) == "final") {
    
    message(paste(methodology, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- variable_weights %>% vectorize_weights(methodology)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic approval fn
    weight_map <-
      try_list %>%
      future_pmap_dfr(~approval_average(approval_polls,
                                        ..2,
                                        ..3,
                                        pull_pollster_weights(variable_weights),
                                        pull_sample_weight(variable_weights),
                                        pull_population_weights(variable_weights),
                                        pull_try_weight(variable_weights, methodology, ..1, "methodology"),
                                        pull_date_weight(variable_weights),
                                        if (type == "approval") yes else no)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(methodology, type)
    
  }
  
}

#################### UPDATE ALL FUNCTION ####################

# util function for messaging user and calling update
call_update_date <- function(type) {
  
  message("Updating `date_weight`.")
  tictoc::tic()
  update_date_weight(type)
  tictoc::toc()
  message("`date_weight` updated.")
  message()
  
}

# util function for messaging user and calling update
call_update_sample <- function(type) {
  
  message("Updating `sample_weight`.")
  tictoc::tic()
  update_sample_weight(type)
  tictoc::toc()
  message("`sample_weight` updated.")
  message()
  
}

# util function for messaging user and calling update
call_update_pollster <- function(pollster, type) {
  
  message(paste("Updating", pollster, "weight."))
  tictoc::tic()
  update_pollster_weight(pollster, type)
  tictoc::toc()
  message(paste(pollster, "updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_offset <- function(pollster, type) {
  
  message(paste("Updating", pollster, "Offset."))
  tictoc::tic()
  update_pollster_offset(pollster, type)
  tictoc::toc()
  message(paste(pollster, "Offset updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_population <- function(population, type) {
  
  message(paste("Updating", population, "weight."))
  tictoc::tic()
  update_population_weight(population, type)
  tictoc::toc()
  message(paste(population, "updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_methodology <- function(methodology, type) {
  
  message(paste("Updating", methodology, "weight."))
  tictoc::tic()
  update_methodology_weight(methodology, type)
  tictoc::toc()
  message(paste(methodology, "updated."))
  message()
  
}

#################### UPDATE ALL FUNCTION #################### 

update_all <- function(type) {
  
  # determine if you want to start from scratch or read in progress
  message("Do you want to initialize a new baseline or read-in current progress?")
  message("[1] Initialize a new baseline")
  message("[2] Read in current progress")
  response <- readline()
  message()
  
  # ask if sure! (don't want to reset progress accidentally!!!)
  if (response == 1) {
    
    message("Are you sure you want to initialize a new baseline? This will lose any progress made.")
    message("If you are sure, please type `Initialize new baseline`")
    response <- readline()
    message()
    
    # init new baseline if sure; return to console if not
    if (response == "Initialize new baseline") {
      
      message("Initializing new baseline for `variable_weights` and `rmse_tracker`.")
      message("This may take a few minutes.")
      message()
      
      # initialize weights & offsets
      variable_weights <- initialize_weights()
      
      # assign based on type 
      if (type == "approval") {
        
        approval_weights <<- variable_weights
        
      } else {
        
        disapproval_weights <<- variable_weights
        
      }
      
      # add multisession plan
      message("Setting up multisession workers.")
      plan(multisession, workers = 8)
      message()
      
      # setup begin/end dates to feed to baseline calc
      begin_baseline <- rep(ymd("2017-01-22"), 1459)
      final_baseline <- seq(ymd("2017-01-23"), ymd("2021-01-20"), "days")
      
      # create baseline results with initialized weights and offsets (1/0)
      message("Creating baseline")
      tictoc::tic()
      baseline <- 
        list(begin = begin_baseline,
             final = final_baseline) %>%
        future_pmap_dfr(~approval_average(approval_polls,
                                          ..1,
                                          ..2,
                                          pull_pollster_weights(variable_weights),
                                          pull_sample_weight(variable_weights),
                                          pull_population_weights(variable_weights),
                                          pull_methodology_weights(variable_weights),
                                          pull_date_weight(variable_weights),
                                          if (type == "approval") yes else no))
      tictoc::toc()
      
      # initialize rmse tracker - build baseline tibble
      rmse_tracker <-
        baseline %>%
        select(-starts_with("ci")) %>%
        left_join(approval_trends, by = "date") %>%
        select(answer, approve_estimate) %>%
        nest(data = everything()) %>%
        mutate(rmse = map(data, yardstick::rmse, truth = approve_estimate, estimate = answer)) %>%
        unnest(rmse) %>%
        select(.estimate) %>%
        rename(rmse = .estimate) %>%
        mutate(index = 0, 
               metric = "baseline",
               weight = 0,
               next_lower = 0,
               next_upper = 0,
               pct_diff = 0, 
               search_suggestion = "baseline") %>%
        relocate(rmse, .after = weight)
      
      # assign based on type
      if (type == "approval") {
        
        approval_rmse_tracker <<- rmse_tracker
        
      } else {
        
        disapproval_rmse_tracker <<- rmse_tracker
        
      }
      
    } else {
      
      message("Update aborted.")
      return()
      
    }
    
  } else {
    
    message("Reading in weights and rmse tracker...")
    message()
    
    # read in data (add to environment) based on type
    # also, assign to function-level variables
    if (type == "approval") {
      
      approval_weights <<- read_csv("data/models/approval/approval_weights.csv")
      approval_rmse_tracker <<- read_csv("data/models/approval/approval_rmse_tracker.csv")
      variable_weights <- approval_weights
      
    } else {
      
      disapproval_weights <<- read_csv("data/models/approval/disapproval_weights.csv")
      disapproval_rmse_tracker <<- read_csv("data/models/approval/disapproval_rmse_tracker.csv")
      variable_weights <- disapproval_weights
      
    }
    
  }
  
  # determine number of updates to be made
  num_updates <-
    variable_weights %>%
    count(search_suggestion) %>%
    filter(search_suggestion == "not final") %>%
    pull(n)
  
  # determine the approximate runtime (~180s per variable on the nonfuture calls)
  runtime <- num_updates * 3
  
  # ask to proceed
  message(paste("Updating all variable weights will take approximately", runtime, "minutes."))
  message("Do you want to continue? (y/n)")
  response <- readline()
  
  message()
  
  # abort if N/n
  if (str_to_lower(response) == "n") {
    
    message("Update aborted.")
    message()
    
  } else {
    
    # setup futures
    message("Setting up multisession workers")
    plan(multisession, workers = 8)
    message()
    
    # update all variables
    # for whatever reason I couldn't get the future_walk calls to work 
    call_update_date(type)
    call_update_sample(type)
    c(pollsters, "Other Pollster") %>% walk(~call_update_pollster(.x, type))
    c(pollsters, "Other Pollster") %>% walk(~call_update_offset(.x, type))
    c("rv", "lv", "a", "v") %>% walk(~call_update_population(.x, type))
    c(methods, "Other Method") %>% walk(~call_update_methodology(.x, type))
    
    # write updates to data/models/approval
    if (type == "approval") {
      
      approval_weights %>% write_csv("data/models/approval/approval_weights.csv")
      approval_rmse_tracker %>% write_csv("data/models/approval/approval_rmse_tracker.csv")
      
    } else {
      
      disapproval_weights %>% write_csv("data/models/approval/disapproval_weights.csv")
      disapproval_rmse_tracker %>% write_csv("data/models/approval/disapproval_rmse_tracker.csv")
      
    }
    
  }
  
}

#################### VIZ FUNCTIONS ####################

# function to visualize trend against fte trend
get_current_fit <- function(.data, type, downweight = 1) {
  
  # setup begin/end dates to feed to baseline calc
  begin_current <- rep(ymd("2017-01-22"), 1459)
  final_current <- seq(ymd("2017-01-23"), ymd("2021-01-20"), "days")
  
  # fit data
  list(begin = begin_current,
       final = final_current) %>%
    future_pmap_dfr(~approval_average(approval_polls,
                                      ..1,
                                      ..2,
                                      pull_pollster_weights(.data),
                                      pull_sample_weight(.data),
                                      pull_population_weights(.data),
                                      pull_methodology_weights(.data),
                                      pull_date_weight(.data),
                                      if (type == "approval") yes else no,
                                      downweight))
  
}

# visualize the current fit
visualize_fit <- function(.data, type) {
  
  # left_join by type
  if (type == "approval") {
    
    fit <- 
      .data %>%
      left_join(approval_trends, by = "date") %>%
      rename(estimate = answer,
             actual = approve_estimate)
    
  } else {
    
    fit <-
      .data %>%
      left_join(disapproval_trends, by = "date") %>%
      rename(estimate = answer,
             actual = disapprove_estimate)
    
  }
  
  fit %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = actual),
              size = 1.1,
              color = "red") +
    geom_ribbon(aes(ymin = ci_lower,
                    ymax = ci_upper),
                fill = "midnightblue",
                alpha = 0.25) +
    geom_line(aes(y = estimate),
              color = "midnightblue",
              size = 1)
  
}

# running plot of rmse drop
visualize_rmse <- function(.data, variable_name = "all") {
  
  # plot all
  if (variable_name == "all") {
    
    .data %>%
      ggplot(aes(x = index,
                 y = rmse)) +
      geom_line(size = 1,
                color = "midnightblue")
    
  } else { # plot a specific variable or variables
    
    .data %>%
      filter(metric %in% variable_name) %>%
      select(-index) %>%
      rowid_to_column() %>%
      ggplot(aes(x = rowid,
                 y = rmse)) + 
      geom_line(size = 1,
                color = "midnightblue")
    
  }
  
}

# create patchwork plot
call_visualizations <- function(type) {
  
  # set variable_weights and rmse_tracker based on type
  if (type == "approval") {
    
    variable_weights <- approval_weights
    rmse_tracker <- approval_rmse_tracker
    
  } else {
    
    variable_weights <- disapproval_weights
    rmse_tracker <- disapproval_rmse_tracker
    
  }
  
  # fit & plot
  interim_fit <<- get_current_fit(variable_weights, type)
  interim_fit %>% visualize_fit(type) / rmse_tracker %>% visualize_rmse()
  
}

#################### MODELTIME - APPROVAL ####################

# set to FALSE to rerun rounds

completed <- TRUE

if (completed == FALSE) {
  
  # round 1
  update_all("approval")
  call_visualizations("approval")
  
  # round 2
  update_all("approval")
  call_visualizations("approval")
  
  # round 3
  update_all("approval")
  call_visualizations("approval")
  
  # round 4
  update_all("approval")
  call_visualizations("approval")
  
  # round 5
  update_all("approval")
  call_visualizations("approval")
  
  # round 6
  update_all("approval")
  call_visualizations("approval")
  
}

#################### MODELTIME - DISAPPROVAL ####################

# set to FALSE to rerun rounds

completed <- TRUE

if (completed == FALSE) {
  
  # round 1
  update_all("disapproval")
  call_visualizations("disapproval")
  
  # round 2
  update_all("disapproval")
  call_visualizations("disapproval")
  
  # round 3
  update_all("disapproval")
  call_visualizations("disapproval")
  
  # round 4
  update_all("disapproval")
  call_visualizations("disapproval")
  
  # round 5
  update_all("disapproval")
  call_visualizations("disapproval")
  
}

#################### DOWNWEIGHT FUNCTIONS ####################

# function for getting the generic downweight for errorbars
update_downweight <- function(lower_bound, upper_bound, type) {
  
  # set variable weights based on type
  if (type == "approval") {
    
    variable_weights <- approval_weights
    confidence_trend <- approval_trends
    
  } else {
    
    variable_weights <- disapproval_weights
    confidence_trend <- disapproval_trends
    
  }
  
  # rename confidence trend cols for merging later on
  confidence_trend <- 
    confidence_trend %>%
    rename(truth = ends_with("estimate"),
           truth_hi = ends_with("hi"),
           truth_lo = ends_with("lo"))
  
  # setup futures
  message("Setting up multisession workers")
  plan(multisession, workers = 8)
  
  # fit weights to data
  weight_map <-
    sequence_weights(lower_bound, upper_bound) %>%
    create_try_list() %>%
    future_pmap_dfr(~approval_average(approval_polls,
                                      ..2,
                                      ..3,
                                      pull_pollster_weights(variable_weights),
                                      pull_sample_weight(variable_weights),
                                      pull_population_weights(variable_weights),
                                      pull_methodology_weights(variable_weights),
                                      pull_date_weight(variable_weights),
                                      if (type == "approval") yes else no,
                                      ..1))
  
  # return summary tibble w/rmse & pct diff
  weight_map %>%
    
    # calculate hi/lo confidence interval
    mutate(est_hi = ci_upper - answer,
           est_lo = ci_lower - answer) %>%
    
    # merging data, light reformatting
    left_join(confidence_trend, by = "date") %>%
    mutate(delta_hi = truth_hi - truth,
           delta_lo = truth_lo - truth) %>%
    bind_cols(weight = sequence_weights(lower_bound, upper_bound)) %>%
    select(weight, 
           starts_with("est"),
           starts_with("delta")) %>%
    
    # get individual rmse for hi/lo confidence interval
    group_by(weight) %>%
    nest() %>%
    mutate(rmse_hi = map(data, yardstick::rmse, truth = delta_hi, estimate = est_hi)) %>%
    unnest(rmse_hi) %>%
    select(-.metric, -.estimator) %>%
    rename(rmse_hi = .estimate) %>%
    mutate(rmse_lo = map(data, yardstick::rmse, truth = delta_lo, estimate = est_lo)) %>%
    unnest(rmse_lo) %>%
    select(-.metric, -.estimator) %>%
    rename(rmse_lo = .estimate) %>%
    select(-data) %>%
    ungroup() %>%
    
    # summarise & return
    mutate(rmse = rmse_hi + rmse_lo,
           pct_diff = (max(rmse) - min(rmse))/mean(max(rmse), min(rmse)))
  
}

# quick function for pulling the downweight
pull_downweight <- function(type) {
  
  # select weight table based on type
  if (type == "approval") {
    
    variable_weights <- approval_weights
    
  } else {
    
    variable_weights <- disapproval_weights
    
  }
  
  weight <- 
    variable_weights %>%
    filter(variable == "downweight") %>%
    pull(weight)
  
  return(weight)
  
}


#################### FIT ERROR BARS - APPROVAL ####################

complete <- TRUE

if (complete == FALSE) {
  
  # run through downweights (there's a better way to do this but I'm lazy)
  downweight <- update_downweight(0, 1, "approval") # best: 0.25
  downweight <- update_downweight(0, 0.5, "approval") # best: 0.125
  downweight <- update_downweight(0, 0.25, "approval") # best: 0.0625
  downweight <- update_downweight(0, 0.125, "approval") # best: 0.03125
  downweight <- update_downweight(0, 0.0625, "approval") # best: 0.015625
  downweight <- update_downweight(0, 0.03125, "approval") # best: 0.015625
  downweight <- update_downweight(0.0078125, 0.0234375, "approval") # best: 0.015625
  downweight <- update_downweight(0.01171875, 0.01953125, "approval") # best: 0.015625
  downweight <- update_downweight(0.013671875, 0.01758125, "approval") # best: 0.016601563
  downweight <- update_downweight(0.015625, 0.017578125, "approval") # best: 0.016113281
  downweight <- update_downweight(0.015625, 0.016601563, "approval") # best: 0.016113281 (final)
  
  downweight <- 0.016113281
  
  # add selected downweight to variable_weights & save to csv
  approval_weights <- 
    approval_weights %>%
    bind_rows(tibble(variable = "downweight",
                     weight = downweight,
                     next_lower = 0,
                     next_upper = 0,
                     search_suggestion = "final"))
  
  approval_weights %>%
    write_csv("data/models/approval/approval_weights.csv")

  }

#################### FIT ERROR BARS - DISAPPROVAL ####################

completed <- TRUE 

if (completed == FALSE) {
  
  # run through disapproval downweights
  downweight <- update_downweight(0, 1, "disapproval") # best: 0.25
  downweight <- update_downweight(0, 0.5, "disapproval") # best: 0.125
  downweight <- update_downweight(0, 0.25, "disapproval") # best: 0.0625
  downweight <- update_downweight(0, 0.125, "disapproval") # best: 0.03125
  downweight <- update_downweight(0, 0.0625, "disapproval") # best: 0.015625
  downweight <- update_downweight(0, 0.03125, "disapproval") # best: 0.0234375
  downweight <- update_downweight(0.015625, 0.03125, "disapproval") # best: 0.01953125
  downweight <- update_downweight(0.015625, 0.0234375, "disapproval") # best: 0.01953125
  downweight <- update_downweight(0.017578125, 0.021484375, "disapproval") # best: 0.01953125
  downweight <- update_downweight(0.018554688, 0.020507813, "disapproval") # best: 0.01953125 (final)
  
  downweight <- 0.01953125
  
  # add selected downweight to disapproval_weights & save to csv
  disapproval_weights <- 
    disapproval_weights %>%
    bind_rows(tibble(variable = "downweight",
                     weight = downweight,
                     next_lower = 0,
                     next_upper = 0,
                     search_suggestion = "final"))
  
  disapproval_weights %>%
    write_csv("data/models/approval/disapproval_weights.csv")
  
}

#################### EXPLORE RESULTS - APPROVAL ####################

completed <- TRUE

if (completed == FALSE) {
  
  # get final fit for plots
  approval_fit <- 
    approval_weights %>%
    get_current_fit("approval",
                    downweight = pull_downweight("approval"))
  
  # visualize final fit
  approval_fit %>%
    visualize_fit("approval") +
    labs(title = "Final fit - approval")
  
  ggsave("plots/approval/training/approval_final_fit.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # visualize rmse drop
  approval_rmse_tracker %>%
    visualize_rmse() +
    labs(title = "rmse - approval")
  
  ggsave("plots/approval/training/approval_rmse_tracker.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # patchwork of approval fit/rmse
  (approval_fit %>% visualize_fit("approval") + labs(title = "Final fit - approval")) / (approval_rmse_tracker %>% visualize_rmse() + labs(title = "rmse - approval"))
  
  ggsave("plots/approval/training/approval_fit_rmse_patchwork.png",
         width = 9,
         height = 12,
         units = "in",
         dpi = 500)
  
  # view pollster weights
  approval_weights %>%
    filter(variable %in% c(pollsters, "Other Pollster")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Pollster weights - approval")
  
  ggsave("plots/approval/training/approval_pollster_weights.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view pollster offsets
  approval_weights %>%
    filter(str_detect(variable, "Offset")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Pollster offset - approval")
  
  ggsave("plots/approval/training/approval_pollster_offsets.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view pollster weight/offset in combination
  approval_weights %>%
    filter(str_detect(variable, "Offset")) %>%
    mutate(variable = str_remove(variable, " Offset")) %>%
    rename(offset = weight) %>%
    select(variable, offset) %>%
    left_join(approval_weights, by = "variable") %>%
    left_join(approval_polls %>% count(pollster), by = c("variable" = "pollster")) %>% 
    ggplot(aes(x = offset,
               y = weight,
               label = variable)) + 
    geom_point(aes(size = n),
               color = "midnightblue",
               alpha = 0.65) +
    ggrepel::geom_label_repel() +
    labs(title = "Pollster Summary - approval")
  
  ggsave("plots/approval/training/approval_pollster_weights_summary_size.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view population weights
  approval_weights %>%
    filter(variable %in% c("rv", "lv", "v", "a")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Population - approval")
  
  ggsave("plots/approval/training/approval_population_weights.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view methodology weights
  approval_weights %>%
    filter(variable %in% c(methods, "Other Method")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Methodology - approval")
  
  ggsave("plots/approval/training/approval_methodology_weights.png",
         width = 9,
         height = 6, 
         units = "in",
         dpi = 500)
  
  # view all variable weights
  approval_weights %>%
    filter(!str_detect(variable, "Offset")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "All weights - approval")
  
  ggsave("plots/approval/training/approval_all_weights.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

#################### EXPLORE RESULTS - DISAPPROVAL ####################

completed <- TRUE

if (completed == FALSE) {
  
  # get final fit for plots
  disapproval_fit <- 
    disapproval_weights %>%
    get_current_fit("disapproval",
                    downweight = pull_downweight("disapproval"))
  
  # visualize final fit
  disapproval_fit %>%
    visualize_fit("disapproval") +
    labs(title = "Final fit - disapproval")
  
  ggsave("plots/approval/training/disapproval_final_fit.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # visualize rmse drop
  disapproval_rmse_tracker %>%
    visualize_rmse() +
    labs(title = "rmse - disapproval")
  
  ggsave("plots/approval/training/disapproval_rmse_tracker.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # better viz for disapproval rmse drop
  disapproval_rmse_tracker %>%
    filter(index != 0) %>%
    visualize_rmse() +
    labs(title = "rmse - disapproval")
  
  ggsave("plots/approval/training/disapproval_rmse_tracker2.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # patchwork of approval fit/rmse
  (disapproval_fit %>% visualize_fit("disapproval") + labs(title = "Final fit - disapproval")) / (disapproval_rmse_tracker %>% filter(index != 0) %>% visualize_rmse() + labs(title = "rmse - disapproval"))
  
  ggsave("plots/approval/training/disapproval_fit_rmse_patchwork.png",
         width = 9,
         height = 12,
         units = "in",
         dpi = 500)
  
  # view pollster weights
  disapproval_weights %>%
    filter(variable %in% c(pollsters, "Other Pollster")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Pollster weights - disapproval")
  
  ggsave("plots/approval/training/disapproval_pollster_weights.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view pollster offsets
  disapproval_weights %>%
    filter(str_detect(variable, "Offset")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Pollster offset - disapproval")
  
  ggsave("plots/approval/training/disapproval_pollster_offsets.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view pollster weight/offset in combination
  disapproval_weights %>%
    filter(str_detect(variable, "Offset")) %>%
    mutate(variable = str_remove(variable, " Offset")) %>%
    rename(offset = weight) %>%
    select(variable, offset) %>%
    left_join(disapproval_weights, by = "variable") %>%
    left_join(approval_polls %>% count(pollster), by = c("variable" = "pollster")) %>% 
    ggplot(aes(x = offset,
               y = weight,
               label = variable)) + 
    geom_point(aes(size = n),
               color = "midnightblue",
               alpha = 0.65) +
    ggrepel::geom_label_repel() +
    labs(title = "Pollster Summary - disapproval")
  
  ggsave("plots/approval/training/disapproval_pollster_weights_summary_size.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view population weights
  disapproval_weights %>%
    filter(variable %in% c("rv", "lv", "v", "a")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Population - disapproval")
  
  ggsave("plots/approval/training/disapproval_population_weights.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # view methodology weights
  disapproval_weights %>%
    filter(variable %in% c(methods, "Other Method")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "Methodology - disapproval")
  
  ggsave("plots/approval/training/disapproval_methodology_weights.png",
         width = 9,
         height = 6, 
         units = "in",
         dpi = 500)
  
  # view all variable weights
  disapproval_weights %>%
    filter(!str_detect(variable, "Offset")) %>%
    mutate(variable = fct_reorder(variable, weight)) %>%
    ggplot(aes(x = variable,
               y = weight)) +
    geom_col(fill = "midnightblue",
             alpha = 0.65) +
    coord_flip() +
    labs(title = "All weights - disapproval")
  
  ggsave("plots/approval/training/disapproval_all_weights.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

#################### EXPLORE COMBINED RESULTS ####################

completed <- TRUE

if (completed == FALSE) {
  
  # get final fit for plots
  approval_fit <- 
    approval_weights %>%
    get_current_fit("approval",
                    downweight = pull_downweight("approval"))
  
  disapproval_fit <-
    disapproval_weights %>%
    get_current_fit("disapproval",
                    downweight = pull_downweight("disapproval"))
  
  # merged plot
  approval_fit %>%
    left_join(disapproval_fit, by = "date") %>%
    rename(approval = answer.x,
           approval_ci_lower = ci_lower.x,
           approval_ci_upper = ci_upper.x,
           disapproval = answer.y,
           disapproval_ci_lower = ci_lower.y,
           disapproval_ci_upper = ci_upper.y) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = disapproval_ci_lower,
                    ymax = disapproval_ci_upper),
                fill = dd_orange,
                alpha = 0.25) +
    geom_ribbon(aes(ymin = approval_ci_lower,
                    ymax = approval_ci_upper),
                fill = dd_green,
                alpha = 0.25) +
    geom_line(aes(y = disapproval),
              color = dd_orange,
              size = 1.1) +
    geom_line(aes(y = approval),
              color = dd_green,
              size = 1.1) +
    labs(x = NULL,
         y = NULL)
  
  ggsave("plots/approval/training/approval_disapproval_historical.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # compare weights against each other
  approval_weights %>%
    left_join(disapproval_weights, by = "variable") %>%
    select(variable, starts_with("weight")) %>%
    rename(approval_weight = weight.x,
           disapproval_weight = weight.y) %>%
    ggplot(aes(x = approval_weight,
               y = disapproval_weight)) +
    geom_point(size = 3.5,
               alpha = 0.65,
               color = "midnightblue") +
    geom_abline(linetype = "dashed",
                size = 1.1,
                color = dd_gray)
  
  ggsave("plots/approval/training/approval_disapproval_weights_comparison.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # historical net approval
  approval_fit %>%
    left_join(disapproval_fit, by = "date") %>%
    mutate(net = answer.x - answer.y,
           net_ci_lower = ci_lower.x - ci_upper.y,
           net_ci_upper = ci_upper.x - ci_lower.y) %>%
    select(date, starts_with("net")) %>%
    ggplot(aes(x = date,
               y = net,
               ymin = net_ci_lower,
               ymax = net_ci_upper)) +
    geom_ribbon(fill = "midnightblue",
                alpha = 0.25) +
    geom_line(color = "midnightblue",
              size = 1.1)
  
  ggsave("plots/approval/training/historical_net_approval.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

#################### EXPLORE ERROR BAR FITS ####################

completed <- TRUE

if (completed == FALSE) {
  
  # get final fit for plots
  approval_fit <- 
    approval_weights %>%
    get_current_fit("approval",
                    downweight = pull_downweight("approval"))
  
  disapproval_fit <-
    disapproval_weights %>%
    get_current_fit("disapproval",
                    downweight = pull_downweight("disapproval"))
  
  # check approval error against trend
  approval_fit %>%
    left_join(approval_trends, by = "date") %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ci_lower,
                    ymax = ci_upper),
                fill = "midnightblue",
                alpha = 0.25) +
    geom_ribbon(aes(ymin = approve_lo,
                    ymax = approve_hi),
                fill = "red",
                alpha = 0.25) +
    geom_line(aes(y = approve_estimate),
              color = "red",
              size = 1.1) +
    geom_line(aes(y = answer),
              color = "midnightblue",
              size = 1) +
    labs(title = "error bar check - approval")
  
  ggsave("plots/approval/training/approval_ci_fit_historical.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # check disapproval ci fit
  disapproval_fit %>%
    left_join(disapproval_trends, by = "date") %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ci_lower,
                    ymax = ci_upper),
                fill = "midnightblue",
                alpha = 0.25) + 
    geom_ribbon(aes(ymin = disapprove_lo,
                    ymax = disapprove_hi),
                fill = "red",
                alpha = 0.25) +
    geom_line(aes(y = disapprove_estimate),
              color = "red",
              size = 1.1) +
    geom_line(aes(y = answer),
              color = "midnightblue",
              size = 1) +
    labs(title = "error bar check - disapproval")
  
  ggsave("plots/approval/training/disapproval_ci_fit_historical.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

#################### CHECK AGAINST CURRENT DATA ####################

completed <- TRUE 

if (completed == FALSE) {
  
  # pull in & wrangle current polls
  approval_polls_current <- 
    read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_approval_polls.csv") %>%
    select(display_name, sample_size, population_full,
           methodology, end_date, yes, no) %>%
    mutate(end_date = mdy(end_date),
           methodology = replace_na(methodology, "Unknown"),
           yes = yes/100,
           no = no/100) %>%
    rename(pollster = display_name) %>%
    mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
           methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
    drop_na()
  
  # pull in & light wrangle of current fte trends
  approval_trends_current <- 
    read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv") %>%
    filter(subgroup == "All polls") %>%
    select(modeldate:disapprove_lo) %>%
    mutate(modeldate = mdy(modeldate),
           across(approve_estimate:disapprove_lo, ~.x/100)) %>%
    rename(date = modeldate)
  
  # create sequence of days to fit against
  final_2022 <- seq(ymd("2021-01-23"), ymd("2022-11-08"), "days")
  begin_2022 <- rep(ymd("2021-01-21"), length(final_2022))
  
  # setup multisession workers
  plan(multisession, workers = 8)
  
  # fit current approval to election day
  fit_2022_approval <-
    list(begin = begin_2022,
         final = final_2022) %>%
    future_pmap_dfr(~approval_average(approval_polls_current,
                                      ..1,
                                      ..2,
                                      pull_pollster_weights(approval_weights),
                                      pull_sample_weight(approval_weights),
                                      pull_population_weights(approval_weights),
                                      pull_methodology_weights(approval_weights),
                                      pull_date_weight(approval_weights),
                                      yes,
                                      pull_downweight("approval")))
  
  # fit current disapproval to election day
  fit_2022_disapproval <-
    list(begin = begin_2022,
         final = final_2022) %>%
    future_pmap_dfr(~approval_average(approval_polls_current,
                                      ..1,
                                      ..2,
                                      pull_pollster_weights(disapproval_weights),
                                      pull_sample_weight(disapproval_weights),
                                      pull_population_weights(disapproval_weights),
                                      pull_methodology_weights(disapproval_weights),
                                      pull_date_weight(disapproval_weights),
                                      no,
                                      pull_downweight("disapproval")))
  
  # check current approval fit
  fit_2022_approval %>%
    filter(date <= Sys.Date()) %>%
    left_join(approval_trends_current, by = "date") %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ci_lower,
                    ymax = ci_upper),
                fill = "midnightblue",
                alpha = 0.25) +
    geom_ribbon(aes(ymin = approve_lo,
                    ymax = approve_hi),
                fill = "red",
                alpha = 0.25) +
    geom_line(aes(y = approve_estimate),
              color = "red",
              size = 1.1) + 
    geom_line(aes(y = answer),
              color = "midnightblue",
              size = 1) +
    labs(title = "Biden's approval comparison - current")
  
  ggsave("plots/approval/training/approval_current_comparison.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # check current disapproval fit
  fit_2022_disapproval %>%
    filter(date <= Sys.Date()) %>%
    left_join(approval_trends_current, by = "date") %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ci_lower,
                    ymax = ci_upper),
                fill = "midnightblue",
                alpha = 0.25) +
    geom_ribbon(aes(ymin = disapprove_lo,
                    ymax = disapprove_hi),
                fill = "red",
                alpha = 0.25) +
    geom_line(aes(y = disapprove_estimate),
              color = "red",
              size = 1.1) +
    geom_line(aes(y = answer),
              color = "midnightblue",
              size = 1) +
    labs(title = "Biden's disapproval comparison - current")
  
  ggsave("plots/approval/training/disapproval_current_comparison.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # check current net approval fit
  fit_2022_approval %>%
    filter(date <= Sys.Date()) %>%
    left_join(fit_2022_disapproval, by = "date") %>%
    mutate(net = answer.x - answer.y,
           net_ci_lower = ci_lower.x - ci_upper.y,
           net_ci_upper = ci_upper.x - ci_lower.y) %>%
    select(date, starts_with("net")) %>%
    left_join(approval_trends_current, by = "date") %>%
    mutate(fte_net = approve_estimate - disapprove_estimate,
           fte_net_ci_lower = approve_lo - disapprove_hi,
           fte_net_ci_upper = approve_hi - disapprove_lo) %>%
    select(date, starts_with("net"), starts_with("fte")) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = net_ci_lower,
                    ymax = net_ci_upper),
                fill = "midnightblue",
                alpha = 0.25) +
    geom_ribbon(aes(ymin = fte_net_ci_lower,
                    ymax = fte_net_ci_upper),
                fill = "red",
                alpha = 0.25) +
    geom_line(aes(y = fte_net),
              color = "red",
              size = 1.1) +
    geom_line(aes(y = net),
              color = "midnightblue",
              size = 1) +
    labs(title = "Biden's net approval comparison - current")
  
  ggsave("plots/approval/training/net_approval_current_comparison.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)  
  
}

#################### PLOT AGAINST NEW DATA ####################

# pull in variable weights
approval_weights <- read_csv("data/models/approval/approval_weights.csv")
disapproval_weights <- read_csv("data/models/approval/disapproval_weights.csv")

# pull in & wrangle current polls
approval_polls_current <- 
  read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_approval_polls.csv") %>%
  select(display_name, sample_size, population_full,
         methodology, end_date, yes, no) %>%
  mutate(end_date = mdy(end_date),
         methodology = replace_na(methodology, "Unknown"),
         yes = yes/100,
         no = no/100) %>%
  rename(pollster = display_name) %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  drop_na()

# create sequence of days to fit against
final_2022 <- seq(ymd("2021-01-23"), ymd("2022-11-08"), "days")
begin_2022 <- rep(ymd("2021-01-21"), length(final_2022))

# setup multisession
plan(multisession, workers = 8)

# fit current approval to election day
fit_2022_approval <-
  list(begin = begin_2022,
       final = final_2022) %>%
  future_pmap_dfr(~approval_average(approval_polls_current,
                                    ..1,
                                    ..2,
                                    pull_pollster_weights(approval_weights),
                                    pull_sample_weight(approval_weights),
                                    pull_population_weights(approval_weights),
                                    pull_methodology_weights(approval_weights),
                                    pull_date_weight(approval_weights),
                                    yes,
                                    pull_downweight("approval")))

# fit current disapproval to election day
fit_2022_disapproval <-
  list(begin = begin_2022,
       final = final_2022) %>%
  future_pmap_dfr(~approval_average(approval_polls_current,
                                    ..1,
                                    ..2,
                                    pull_pollster_weights(disapproval_weights),
                                    pull_sample_weight(disapproval_weights),
                                    pull_population_weights(disapproval_weights),
                                    pull_methodology_weights(disapproval_weights),
                                    pull_date_weight(disapproval_weights),
                                    no,
                                    pull_downweight("disapproval")))

# merge frames
fit_2022 <-
  fit_2022_approval %>%
  left_join(fit_2022_disapproval, by = "date") %>%
  rename(approval = answer.x,
         approval_lo = ci_lower.x,
         approval_hi = ci_upper.x,
         disapproval = answer.y,
         disapproval_lo = ci_lower.y,
         disapproval_hi = ci_upper.y)

# get current approval/disapproval estimates
current_approval <- 
  fit_2022 %>%
  filter(date == Sys.Date()) %>%
  pull(approval)

current_disapproval <-
  fit_2022 %>%
  filter(date == Sys.Date()) %>%
  pull(disapproval)

current_net <- current_approval - current_disapproval

# generate list of dates for which we have polls
plot_dates <- 
  approval_polls_current %>%
  distinct(end_date) %>%
  pull(end_date)

# reformat approval_polls_current for plotting
approval_polls_current <- 
  approval_polls_current %>%
  select(end_date, pollster, yes, no) %>%
  mutate(pollster = paste(pollster, "Offset")) %>%
  left_join(approval_weights, by = c("pollster" = "variable")) %>%
  mutate(yes = yes + weight) %>%
  select(end_date:no) %>%
  left_join(disapproval_weights, by = c("pollster" = "variable")) %>%
  mutate(no = no + weight) %>%
  select(end_date:no)

# plot approval/disapproval
fit_2022 %>%
  mutate(across(approval:disapproval_hi, ~if_else(date > Sys.Date(), as.double(NA), .x)),
         across(ends_with("lo"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA))),
         across(ends_with("hi"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA)))) %>%
  left_join(approval_polls_current, by = c("date" = "end_date")) %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = disapproval_lo,
                  ymax = disapproval_hi),
              fill = dd_orange,
              alpha = 0.25,
              na.rm = TRUE) + 
  geom_ribbon(aes(ymin = approval_lo,
                  ymax = approval_hi),
              fill = dd_green,
              alpha = 0.25, 
              na.rm = TRUE) +
  geom_point(aes(y = yes),
             color = dd_green,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_point(aes(y = no),
             color = dd_orange,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_line(aes(y = disapproval),
            color = "white",
            size = 3) +
  geom_line(aes(y = disapproval),
            color = dd_orange,
            size = 1.25) +
  geom_line(aes(y = approval),
            color = "white",
            size = 3) +
  geom_line(aes(y = approval),
            color = dd_green,
            size = 1.25) +
  geom_vline(xintercept = Sys.Date(),
             size = 1,
             linetype = "dotted",
             color = "gray") +
  geom_shadowtext(x = Sys.Date() + 45,
                  y = current_disapproval,
                  label = paste0(round(current_disapproval, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = dd_orange,
                  bg.color = "white") +
  geom_shadowtext(x = Sys.Date() + 45,
                  y = current_approval,
                  label = paste0(round(current_approval, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = dd_green,
                  bg.color = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(labels = scales::date_format("%b"),
               breaks = "2 months") +
  theme(panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(family = "Roboto Slab"),
        plot.background = element_rect(fill = "white",
                                       color = "white")) +
  labs(title = "Do Voters <span style=color:'#65D755'>**Approve**</span> or <span style=color:'#F48847'>**Disapprove**</span> of Joe Biden's Performance?",
       subtitle = paste("Estimated presidential approval and disapproval as of",
                        format(Sys.Date(), "%b %d")),
       x = NULL,
       y = NULL,
       caption = paste0("Model by @markjrieke\n",
                        "Data courtesy of @FiveThirtyEight\n",
                        "https://projects.fivethirtyeight.com/biden-approval-rating/"))

# save over current photo
ggsave("plots/approval/approval_disapproval_current.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

# plot net approval
fit_2022 %>%
  mutate(across(approval:disapproval_hi, ~if_else(date > Sys.Date(), as.double(NA), .x)),
         across(ends_with("lo"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA))),
         across(ends_with("hi"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA)))) %>%
  left_join(approval_polls_current, by = c("date" = "end_date")) %>%
  mutate(net = approval - disapproval,
         net_lo = approval_lo - disapproval_hi,
         net_hi = approval_hi - disapproval_lo,
         net_poll = yes - no) %>%
  select(date, starts_with("net")) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = net_lo,
                  ymax = net_hi),
              fill = dd_purple,
              alpha = 0.25,
              na.rm = TRUE) +
  geom_point(aes(y = net_poll),
             color = dd_purple,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_line(aes(y = net),
            color = "white",
            size = 3) + 
  geom_line(aes(y = net),
            color = dd_purple,
            size = 1.25) +
  geom_vline(xintercept = Sys.Date(),
             size = 1,
             linetype = "dotted",
             color = "gray") +
  geom_shadowtext(x = Sys.Date() + 45,
                  y = current_net,
                  label = paste0(round(current_net, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = dd_purple,
                  bg.color = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(labels = scales::date_format("%b"),
               breaks = "2 months") +
  theme(panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(family = "Roboto Slab"),
        plot.background = element_rect(fill = "white",
                                       color = "white")) +
  labs(title = "How <span style=color:'#D755A6'>**Popular**</span> is Joe Biden?",
       subtitle = paste("Estimated presidential net approval as of",
                        format(Sys.Date(), "%b %d")),
       x = NULL,
       y = NULL,
       caption = paste0("Model by @markjrieke\n",
                        "Data courtesy of @FiveThirtyEight\n",
                        "https://projects.fivethirtyeight.com/biden-approval-rating/"))

# save over current photo
ggsave("plots/approval/net_approval_current.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

#################### TESTING ZONE DAWG ####################

completed <- TRUE

if (completed == FALSE) {
  
  # example of projecting out into the future
  fit_2022 %>%
    filter(date <= Sys.Date() + 60) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = disapproval_lo,
                    ymax = disapproval_hi),
                fill = dd_orange,
                alpha = 0.25) +
    geom_ribbon(aes(ymin = approval_lo,
                    ymax = approval_hi),
                fill = dd_green,
                alpha = 0.25) +
    geom_line(aes(y = disapproval),
              color = dd_orange,
              size = 1.1) +
    geom_line(aes(y = approval),
              color = dd_green,
              size = 1.1) +
    labs(x = NULL,
         y = NULL) +
    theme(plot.background = element_rect(fill = "white",
                                         color = "white"))
  
  ggsave("plots/approval/training/projection.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # check what polls make up the most weight
  approval_polls_current %>%
    
    # apply approval pollster offsets
    mutate(pollster = paste(pollster, "Offset")) %>%
    left_join(approval_weights, by = c("pollster" = "variable")) %>%
    mutate(yes = yes + weight,
           yes_votes = round(yes * sample_size),
           not_yes_votes = round((1-yes) * sample_size)) %>%
    select(-weight, -next_lower, -next_upper, -search_suggestion) %>%
    mutate(pollster = str_remove_all(pollster, " Offset")) %>%
    
    # apply approval pollster weight
    left_join(approval_weights, by = c("pollster" = "variable")) %>%
    select(-next_lower, -next_upper, -search_suggestion) %>%
    rename(pollster_weight = weight) %>%
    
    # apply approval sample size weight
    mutate(sample_weight = log10(sample_size) * pull_sample_weight(approval_weights)) %>%
    
    # apply approval population weight
    left_join(approval_weights, by = c("population_full" = "variable")) %>%
    select(-next_lower, -next_upper, -search_suggestion) %>%
    rename(population_weight = weight) %>%
    
    # apply approval methodology weight
    left_join(approval_weights, by = c("methodology" = "variable")) %>%
    select(-next_lower, -next_upper, -search_suggestion) %>%
    rename(methodology_weight = weight) %>%
    
    # apply approval date weight
    mutate(days_diff = as.numeric(Sys.Date() - end_date) + 1,
           weeks_diff = days_diff/7,
           date_weight = pull_date_weight(approval_weights) ^ weeks_diff) %>%
    select(-days_diff, -weeks_diff) %>%
    
    # summarise approval total weight
    mutate(approval_alpha = yes_votes * pollster_weight * sample_weight * methodology_weight * date_weight,
           approval_beta = not_yes_votes * pollster_weight * sample_weight * population_weight * methodology_weight * date_weight) %>%
    select(pollster:no, starts_with("approval")) %>%
    
    # apply disapproval pollster offsets
    mutate(pollster = paste(pollster, "Offset")) %>%
    left_join(disapproval_weights, by = c("pollster" = "variable")) %>%
    mutate(no = no + weight,
           no_votes = round(no * sample_size),
           not_no_votes = round((1-no) * sample_size)) %>%
    select(-weight, -next_lower, -next_upper, -search_suggestion) %>%
    mutate(pollster = str_remove_all(pollster, " Offset")) %>%
    
    # apply disapproval pollster weight
    left_join(disapproval_weights, by = c("pollster" = "variable")) %>%
    select(-next_lower, -next_upper, -search_suggestion) %>%
    rename(pollster_weight = weight) %>%
    
    # apply disapproval sample size weight
    mutate(sample_weight = log10(sample_size) * pull_sample_weight(disapproval_weights)) %>%
    
    # apply disapproval population weight
    left_join(disapproval_weights, by = c("population_full" = "variable")) %>%
    select(-next_lower, -next_upper, -search_suggestion) %>%
    rename(population_weight = weight) %>%
    
    # apply disapproval methodology weight
    left_join(disapproval_weights, by = c("methodology" = "variable")) %>%
    select(-next_lower, -next_upper, -search_suggestion) %>%
    rename(methodology_weight = weight) %>%
    
    # apply disapproval date weight
    mutate(days_diff = as.numeric(Sys.Date() - end_date) + 1,
           weeks_diff = days_diff/7,
           date_weight = pull_date_weight(disapproval_weights) ^ weeks_diff) %>%
    select(-days_diff, -weeks_diff) %>%
    
    # summarise approval total weight
    mutate(disapproval_alpha = no_votes * pollster_weight * sample_weight * methodology_weight * date_weight,
           disapproval_beta = not_no_votes * pollster_weight * sample_weight * population_weight * methodology_weight * date_weight) %>%
    select(pollster:no, starts_with("approval"), starts_with("disapproval")) %>%
    
    # summarise total weight
    mutate(total_weight = approval_alpha + approval_beta + disapproval_alpha + disapproval_beta) %>%
    select(pollster:no, starts_with("total")) %>%
    filter(end_date <= ymd("2021-12-18")) %>%
    mutate(relative_weight = total_weight/sum(total_weight)) %>%
    arrange(desc(relative_weight)) %>%
    select(-total_weight)
  
  # net approval method comparison
  approval_polls_current %>%
    mutate(net = yes - no) %>%
    group_by(end_date) %>%
    summarise(daily_mean = mean(net),
              daily_median = median(net)) %>%
    ungroup() %>%
    mutate(rollavg_mean = zoo::rollmean(daily_mean, 7, na.pad = TRUE, align = "right"),
           rollavg_median = zoo::rollmean(daily_median, 7, na.pad = TRUE, align = "right")) %>%
    select(end_date, starts_with("roll")) %>%
    left_join(approval_polls_current, by = "end_date") %>%
    mutate(net = yes - no) %>%
    select(-(pollster:no)) %>%
    left_join(fit_2022, by = c("end_date" = "date")) %>%
    mutate(net_est = approval - disapproval) %>%
    select(-(approval:disapproval_hi)) %>%
    pivot_longer(starts_with("roll"),
                 names_to = "method",
                 values_to = "poll_avg") %>%
    ggplot(aes(x = end_date)) +
    geom_point(aes(y = net),
               color = dd_purple,
               size = 2,
               alpha = 0.25) +
    geom_smooth(aes(y = net),
                method = "loess",
                formula = y ~ x) +
    geom_line(aes(y = poll_avg,
                  color = method),
              size = 1.1,
              alpha = 0.6) +
    geom_line(aes(y = net_est),
              color = dd_purple,
              size = 1.1) +
    theme(plot.background = element_rect(fill = "white",
                                         color = "white"))
  
  ggsave("plots/approval/training/methods_compare.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

