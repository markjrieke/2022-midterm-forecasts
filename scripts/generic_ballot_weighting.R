# libraries ----
library(tidyverse)
library(lubridate)
library(furrr)
library(riekelib)

#################### SETUP ####################

# setup parallel processing (Windows) ----
num_cores <- parallel::detectCores()
clusters <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(clusters)

# set path ----
path <- "data/polls/src/fte/"

# load data ----
generic_polls <- read_csv(paste0(path, "generic_ballot_polls_historical.csv"))
generic_trend <- read_csv(paste0(path, "generic_ballot.csv"))

# pre function wrangle ----

# parse down frames to only useful cols; light reformatting
generic_polls <- 
  generic_polls %>%
  select(cycle, display_name, sample_size, population_full, 
         methodology, end_date, dem, rep, ind) %>%
  mutate(end_date = mdy(end_date),
         ind = replace_na(ind, 0),
         methodology = replace_na(methodology, "Unknown"),
         sample_size = round((dem + rep)/100 * sample_size),
         dem2pv = dem/(dem + rep),
         dem_votes = round(dem2pv * sample_size),
         rep_votes = round((1-dem2pv) * sample_size)) %>%
  select(-dem, -rep, -ind, dem2pv) %>%
  drop_na() %>%
  rename(pollster = display_name)
  
generic_trend <- 
  generic_trend %>%
  select(date, dem_estimate, rep_estimate) %>%
  mutate(dem2pv = dem_estimate/(dem_estimate + rep_estimate)) %>%
  select(date, dem2pv)

# create list of pollsters who have conducted at least 5 generic ballot polls
pollsters <- 
  generic_polls %>%
  count(pollster) %>%
  arrange(desc(n)) %>%
  filter(n >= 5) %>%
  pull(pollster)

# create list of methodologies that have been utilized at least 5 times
methods <-
  generic_polls %>%
  count(methodology) %>%
  arrange(desc(n)) %>%
  filter(n >= 5) %>%
  pull(methodology)

# replace pollsters/methodologies that don't occur often with "Other"
generic_polls <- 
  generic_polls %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  drop_na()

# create vectors for 2018 cycle
begin_2018 <- rep(ymd("2016-11-04"), 571)
final_2018 <- seq(ymd("2017-04-15"), ymd("2018-11-06"), "days")

# create vectors for 2020 cycle
begin_2020 <- rep(ymd("2018-11-07"), 582)
final_2020 <- seq(ymd("2019-04-01"), ymd("2020-11-02"), "days")

# create combined beginning & final vectors
begin <- c(rep(begin_2018, 5), rep(begin_2020, 5))
final <- c(rep(final_2018, 5), rep(final_2020, 5))

#################### GENERIC BALLOT AVERAGE FUNCTION ####################

#' Return the weighted polling average of polls conducted from `begin_date` to `end_date`
#' 
#' @param begin_date earliest date to include polls, by polling period end_date.
#' @param final_date last date to include polls, by polling period end_date.
#' @param pollster_weight tibble of pollster weights and offsets
#' @param sample_weight sample size weight (relative to a sample size of 1000)
#' @param population_weight tibble of weights by survey population
#' @param method_weight tibble of weights by survey methodology
#' @param date_weight weight for exponential decay function
generic_ballot_average <- function(begin_date,
                                   final_date,
                                   pollster_weight,
                                   sample_weight,
                                   population_weight,
                                   method_weight,
                                   date_weight) {
  
  generic_polls %>%
  
    # filter to just the relevant dates  
    filter(end_date <= final_date,
           end_date >= begin_date) %>%
    
    # apply pollster weights and offsets 
    left_join(pollster_weight, by = "pollster") %>%
    mutate(dem2pv = dem2pv + pollster_offset,
           dem_votes = round(dem2pv * sample_size),
           rep_votes = round((1-dem2pv) * sample_size)) %>%
    select(-pollster_offset) %>%
    
    # apply sample size weight
    mutate(sample_weight = sample_size/1000 * sample_weight) %>%
    
    # apply population weight
    left_join(population_weight, by = "population_full") %>%
    
    # apply methodology weight
    left_join(method_weight, by = "methodology") %>%
    
    # apply date weight
    mutate(days_diff = as.numeric(final_date - end_date) + 1,
           weeks_diff = days_diff/7,
           date_weight = date_weight ^ weeks_diff) %>%
    select(-days_diff, -weeks_diff) %>%
    
    # created individual poll weights
    mutate(alpha = dem_votes * pollster_weight * sample_weight * population_weight * method_weight * date_weight,
           beta = rep_votes * pollster_weight * sample_weight * population_weight * method_weight * date_weight) %>%
    
    # summarise with a weak uniform prior
    summarise(alpha = sum(alpha) + 1,
              beta = sum(beta) + 1) %>%
    mutate(dem2pv = alpha/(alpha + beta),
           date = final_date) %>%
    beta_interval(alpha, beta) %>%
    select(date, dem2pv, ci_lower, ci_upper)
}

#################### UTIL FUNCTIONS #################### 

# check if variable_weight search suggestion is final or ought to continue
check_suggestion <- function(variable_name) {
  
  variable_weights %>%
    filter(variable == variable_name) %>%
    pull(search_suggestion)
  
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
pull_date_weight <- function() {
  
  variable_weights %>%
    filter(variable == "date_weight") %>%
    pull(weight)
  
} 
  
# pull sample_size weight
pull_sample_weight <- function() {
  
  variable_weights %>%
    filter(variable == "sample_size") %>%
    pull(weight)
  
}

# util function to create a temporary tibble replacing a current weight with a "try" weight
pull_try_weight <- function(variable_name, new_weight, type) {
  
  # create a new weight table to pass to one of the pull functions
  new_weight_tibble <- 
    variable_weights %>%
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

# function to pull the next lower or next upper bound
pull_bound <- function(variable_name, type) {
  
  if (type == "lower") {
    
    variable_weights %>%
      filter(variable == variable_name) %>%
      pull(next_lower)
    
  } else {
    
    variable_weights %>%
      filter(variable == variable_name) %>%
      pull(next_upper)
    
  }
    
  
}

# create sequence of new weights to try
sequence_weights <- function(lower_bound, upper_bound) {
  
  try_weight <- seq(lower_bound, upper_bound, length.out = 5)
  try_weight_2018 <- try_weight %>% rep(571) %>% as_tibble() %>% arrange(value) %>% pull(value)
  try_weight_2020 <- try_weight %>% rep(582) %>% as_tibble() %>% arrange(value) %>% pull(value)
  
  weights <- c(try_weight_2018, try_weight_2020)
  
  return(weights)
  
}

# create vector of weights to be passed to try_list
vectorize_weights <- function(variable_name) {
  
  # generate lower & upper bounds
  lower_bound <- 
    variable_name %>%
    pull_bound("lower")
  
  upper_bound <-
    variable_name %>%
    pull_bound("upper")
  
  # create a sequence of new weights to try
  weights <- sequence_weights(lower_bound, upper_bound)
  
  return(weights)
  
}

# create list of inputs to map to generic_ballot_average
create_try_list <- function(weights) {
  
  try_list <- 
    list(weights = weights,
         begin = begin,
         final = final)
  
  return(try_list)
  
}

# summarise results & recommend new bounds
summarise_weights <- function(.data, metric) {
  
  # create tibble of rmse for each weight evaluated
  weight_metrics <- 
    .data %>%
    select(-starts_with("ci")) %>%
    
    # join fte trendline
    left_join(generic_trend, by = "date") %>%
    drop_na() %>%
    rename(est = dem2pv.x,
           act = dem2pv.y) %>%
    select(-date) %>%
    
    # calculate each weight's rmse
    group_by(weight) %>%
    nest() %>%
    mutate(rmse = map(data, yardstick::rmse, truth = act, estimate = est)) %>%
    unnest(rmse) %>%
    ungroup() %>%
    rowid_to_column() %>%
    select(rowid, weight, .estimate) %>%
    rename(rmse = .estimate)
  
  # find weight that gives smallest rmse
  best_weight <- 
    weight_metrics %>%
    filter(rmse == min(rmse)) %>%
    pull(weight)
  
  # pull the smallest rmse for tracking
  best_rmse <- 
    weight_metrics %>%
    filter(rmse == min(rmse)) %>%
    pull(rmse)
  
  # pull the row index that gave the best rmse
  best_index <- 
    weight_metrics %>%
    filter(weight == best_weight) %>%
    pull(rowid)
  
  # get the step between each weight 
  delta <-
    weight_metrics %>%
    mutate(delta = weight - lag(weight)) %>%
    drop_na() %>%
    distinct(delta) %>%
    pull(delta)
  
  # assign next_upper & next_lower
  if (best_index == 1) {
    
    next_lower <- weight_metrics %>% filter(rowid == 1) %>% pull(weight) - delta
    next_upper <- weight_metrics %>% filter(rowid == 2) %>% pull(weight)
    
  } else if (best_index == 5) {
    
    next_lower <- weight_metrics %>% filter(rowid == 4) %>% pull(weight)
    next_upper <- weight_metrics %>% filter(rowid == 5) %>% pull(weight) + delta
    
  } else {
    
    next_lower <- weight_metrics %>% filter(rowid == best_index - 1) %>% pull(weight)
    next_upper <- weight_metrics %>% filter(rowid == best_index + 1) %>% pull(weight)
    
  }
  
  # check for % difference between best and worst rmse
  worst_rmse <-
    weight_metrics %>%
    filter(rmse == max(rmse)) %>%
    pull(rmse)
  
  pct_diff <- abs(worst_rmse - best_rmse)/mean(c(best_rmse, worst_rmse))
  
  # note whether or not to continue to search for better weights, based off difference threshold
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
update_rmse_tracker <- function(.data) {
  
  # <<- interacts with the global object 
  rmse_tracker <<- 
    rmse_tracker %>%
    
    # pull current index
    filter(index == max(index)) %>%
    select(index) %>%
    
    # increase index
    mutate(index = index + 1) %>%
    
    # add index to data
    bind_cols(.data) %>%
    
    # bind back to rmse_tracker
    bind_rows(rmse_tracker) %>%
    arrange(index)
  
}

# function to update the variable weight table
update_weight_table <- function(.data, variable_name) {
  
  # <<- interact with global environment
  variable_weights <<- 
    variable_weights %>%
    filter(variable != variable_name) %>%
    
    # reformat & bind .data with new weights/suggestions
    bind_rows(.data %>% select(-rmse, -pct_diff) %>% rename(variable = metric))
  
}

# function to update all running tables
update_tables <- function(.data, variable_name) {
  
  # summarise results based on best rmse
  weight_summary <-
    .data %>%
    summarise_weights(variable_name)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
  # update weight table
  weight_summary %>% update_weight_table(variable_name)
  
}

#################### WEIGHT UPDATE FUNCTIONS ####################

# function to update the weight for exponential decay by date function
update_date_weight <- function() {
  
  # do not evaluate if weight is final
  if (check_suggestion("date_weight") == "final") {
    
    message("date_weight marked as final and will not be updated.")
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- vectorize_weights("date_weight")
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic_ballot_average
    weight_map <-
      try_list %>%
      future_pmap_dfr(~generic_ballot_average(..2,
                                              ..3,
                                              pull_pollster_weights(variable_weights),
                                              pull_sample_weight(),
                                              pull_population_weights(variable_weights),
                                              pull_methodology_weights(variable_weights),
                                              ..1)) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables("date_weight")
    
  }
  
}

# function to update the weight based on sample size
update_sample_weight <- function() {
  
  # do not evaluate if weight is final
  if (check_suggestion("sample_size") == "final") {
    
    message("sample_size marked as final and will not be updated.")
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- vectorize_weights("sample_size")
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic_ballot_average
    weight_map <-
      try_list %>%
      future_pmap_dfr(~generic_ballot_average(..2,
                                              ..3,
                                              pull_pollster_weights(variable_weights),
                                              ..1,
                                              pull_population_weights(variable_weights),
                                              pull_methodology_weights(variable_weights),
                                              pull_date_weight())) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables("sample_size")
    
  }
  
}

# function to update the weight based on an individual pollster
update_pollster_weight <- function(pollster) {
  
  # do not evaluate if weight is final
  if (check_suggestion(pollster) == "final") {
    
    message(paste(pollster, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- vectorize_weights(pollster)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic_ballot_average
    weight_map <-
      try_list %>%
      future_pmap_dfr(~generic_ballot_average(..2,
                                              ..3,
                                              pull_try_weight(pollster, ..1, "pollster"),
                                              pull_sample_weight(),
                                              pull_population_weights(variable_weights),
                                              pull_methodology_weights(variable_weights),
                                              pull_date_weight())) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(pollster)
    
  }
  
}

# function to update a pollster's offset
update_pollster_offset <- function(pollster) {
  
  # create offset variable
  offset <- paste(pollster, "Offset")
  
  # do not evaluate if weight is final
  if (check_suggestion(offset) == "final") {
    
    message(paste(offset, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- vectorize_weights(offset)
    
    # create a list of vetors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic_ballot_average
    weight_map <-
      try_list %>%
      future_pmap_dfr(~generic_ballot_average(..2,
                                              ..3,
                                              pull_try_weight(offset, ..1, "pollster"),
                                              pull_sample_weight(),
                                              pull_population_weights(variable_weights),
                                              pull_methodology_weights(variable_weights),
                                              pull_date_weight())) %>%
      bind_cols(weight = weights) 
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(offset)
    
  }
  
}

# function to update weight based on population type
update_population_weight <- function(population) {
  
  # do not evaluate if weight is final
  if (check_suggestion(population) == "final") {
    
    message(paste(population, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- vectorize_weights(population)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic_ballot_average
    weight_map <-
      try_list %>%
      future_pmap_dfr(~generic_ballot_average(..2,
                                              ..3,
                                              pull_pollster_weights(variable_weights),
                                              pull_sample_weight(),
                                              pull_try_weight(population, ..1, "population"),
                                              pull_methodology_weights(variable_weights),
                                              pull_date_weight())) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(population)
    
  }
  
}

# function to update weight based on methodology
update_methodology_weight <- function(methodology) {
  
  # do not evaluate if weight is final
  if (check_suggestion(methodology) == "final") {
    
    message(paste(methodology, "marked as final and will not be updated."))
    
  } else {
    
    # create a vector of weights to bind to results
    weights <- vectorize_weights(methodology)
    
    # create a list of vectors to map against
    try_list <- weights %>% create_try_list()
    
    # map inputs to generic_ballot_average
    weight_map <-
      try_list %>%
      future_pmap_dfr(~generic_ballot_average(..2,
                                              ..3,
                                              pull_pollster_weights(variable_weights),
                                              pull_sample_weight(),
                                              pull_population_weights(variable_weights),
                                              pull_try_weight(methodology, ..1, "methodology"),
                                              pull_date_weight())) %>%
      bind_cols(weight = weights)
    
    # update rmse & variable weight tables
    weight_map %>% update_tables(methodology)
    
  }
  
}

#################### UPDATE ALL FUNCTION ####################

# util function for messaging user & calling update
call_update_date <- function() {
  
  message("Updating `date_weight`.")
  tictoc::tic()
  update_date_weight()
  tictoc::toc()
  message("`date_weight` updated.")
  message()
  
}

# util function for messaging user & calling update
call_update_sample <- function() {
  
  message("Updating `sample_weight`.")
  tictoc::tic()
  update_sample_weight()
  tictoc::toc()
  message("`sample_weight` updated.")
  message()
  
}

# util function for messaging user & calling update
call_update_pollster <- function(pollster) {
  
  message(paste("Updating", pollster, "weight."))
  tictoc::tic()
  update_pollster_weight(pollster)
  tictoc::toc()
  message(paste(pollster, "updated."))
  message()
  
}

# util function for messaging user & calling update
call_update_offset <- function(pollster) {
  
  message(paste("Updating", pollster, "Offset."))
  tictoc::tic()
  update_pollster_offset(pollster)
  tictoc::toc()
  message(paste(pollster, "Offset updated."))
  message()
  
}

# util function for messaging user & calling update
call_update_population <- function(population) {
  
  message(paste("Updating", population, "weight."))
  tictoc::tic()
  update_population_weight(population)
  tictoc::toc()
  message(paste(population, "updated."))
  message()
  
}

# util function for messaging user & calling update
call_update_methodology <- function(methodology) {
  
  message(paste("Updating", methodology, "weight."))
  tictoc::tic()
  update_methodology_weight(methodology)
  tictoc::toc()
  message(paste(methodology, "updated."))
  message()
  
}


update_all <- function() {
  
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
    if(response == "Initialize new baseline") {
      
      message("Initializing new baseline for `variable_weights` and `rmse_tracker`.")
      message("This will take approximately one minute.")
      message()
      
      # initialize variable weights & offsets
      variable_weights <<- 
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
          tibble(variable = generic_polls %>% distinct(population_full) %>% pull(population_full),
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
      
      # add multisession plan
      message("Setting up multisession workers.")
      plan(multisession, workers = 8)
      message()
      
      message("Creating baseline")
      # create baseline results with initialized weights and offsets (1/0)
      baseline <- 
        list(begin = c(begin_2018, begin_2020),
             final = c(final_2018, final_2020)) %>%
        pmap_dfr(~generic_ballot_average(..1,
                                         ..2,
                                         pull_pollster_weights(variable_weights),
                                         pull_sample_weight(),
                                         pull_population_weights(variable_weights),
                                         pull_methodology_weights(variable_weights),
                                         pull_date_weight()))
      
      # initialize rmse tracker - build baseline tibble
      rmse_tracker <<- 
        baseline %>%
        select(-starts_with("ci")) %>%
        left_join(generic_trend, by = "date") %>%
        rename(est = dem2pv.x,
               act = dem2pv.y) %>%
        select(-date) %>%
        nest(data = everything()) %>%
        mutate(rmse = map(data, yardstick::rmse, truth = act, estimate = est)) %>%
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
      
    } else {
      
      
      message("Update aborted.")
      return()
      
    }
    
  } else {
    
    message("Reading in weights and rmse tracker...")
    message()
    
    # read in data (add to environment)
    variable_weights <<- read_csv("data/models/generic_ballot/variable_weights.csv")
    rmse_tracker <<- read_csv("data/models/generic_ballot/rmse_tracker.csv")
    
  }
  
  # determine number of updates to be made
  num_updates <- 
    variable_weights %>%
    count(search_suggestion) %>%
    filter(search_suggestion == "not final") %>%
    pull(n)
  
  # determine the approximate runtime (~75s per variable)
  runtime <- round(num_updates * 75/60)
  
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
    
    # update all variables
    call_update_date()
    call_update_sample()
    c(pollsters, "Other Pollster") %>% walk(call_update_pollster)
    c(pollsters, "Other Pollster") %>% walk(call_update_offset)
    c("rv", "lv", "a", "v") %>% walk(call_update_population)
    c(methods, "Other Method") %>% walk(call_update_methodology)
    
    # write updates to data/models/generic_ballot
    variable_weights %>% write_csv("data/models/generic_ballot/variable_weights.csv")
    rmse_tracker %>% write_csv("data/models/generic_ballot/rmse_tracker.csv")
    
  }
  
}


##################### TESTING ZONE DAWG #######################

