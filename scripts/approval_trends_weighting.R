# libraries ----
library(tidyverse)
library(lubridate)
library(furrr)
library(riekelib)
library(patchwork)
library(shadowtext)

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
    weights <- variable_weights %>% vectorize_weights("sample_size")
    
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
               metric = "bseline",
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
  
  # determine the approximate runtime (~120s per variable)
  runtime <- num_updates * 2
  
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
    call_update_date(type)
    call_update_sample(type)
    c(pollsters, "Other Pollster") %>% future_walk(~call_update_pollster(.x, type))
    c(pollsters, "Other Pollster") %>% future_walk(~call_update_offset(.x, type))
    c("rv", "lv", "a", "v") %>% future_walk(~call_update_population(.x, type))
    c(methods, "Other Method") %>% future_walk(~call_update_methodology(.x, type))
    
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



#################### TESTING ZONE DAWG ####################

# initialize variable weights & offsets
approval_weights <- initialize_weights()

# initialize variable weights & offsets
disapproval_weights <- initialize_weights()

begin_date <- ymd("2017-01-22")
final_date <- ymd("2021-01-20")
approval_pollster_weights <- pull_pollster_weights(approval_weights)
approval_sample_weight <- pull_sample_weight(approval_weights)
approval_population_weight <- pull_population_weights(approval_weights)
approval_date_weight <- pull_date_weight(approval_weights)
approval_method_weight <- pull_methodology_weights(approval_weights)


test_type <- "disapproval"



approval_average(approval_polls, 
                 begin_date, 
                 final_date, 
                 pull_pollster_weights(approval_weights),
                 pull_sample_weight(approval_weights),
                 pull_population_weights(approval_weights),
                 pull_methodology_weights(approval_weights),
                 pull_date_weight(approval_weights),
                 if (test_type == "approval") yes else no)

begin_test <- rep(ymd("2017-01-22"), 1459)
final_test <- seq(ymd("2017-01-23"), ymd("2021-01-20"), "days")
                
plan(multisession, workers = 8)

baseline <- 
  list(begin = begin_test,
       final = final_test) %>%
  future_pmap_dfr(~approval_average(approval_polls,
                                    ..1,
                                    ..2,
                                    pull_pollster_weights(approval_weights),
                                    pull_sample_weight(approval_weights),
                                    pull_population_weights(approval_weights),
                                    pull_methodology_weights(approval_weights),
                                    pull_date_weight(approval_weights),
                                    yes))

approval_rmse_tracker <- 
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
         metric = "bseline",
         weight = 0,
         next_lower = 0,
         next_upper = 0,
         pct_diff = 0, 
         search_suggestion = "baseline") %>%
  relocate(rmse, .after = weight)

update_date_weight("approval")

disapproval_rmse_tracker <- approval_rmse_tracker

call_update_sample("disapproval")








