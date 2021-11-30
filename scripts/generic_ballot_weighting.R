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

# initialize variable weights & offsets
variable_weights <- 
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

# create vectors for 2018 cycle
begin_2018 <- rep(ymd("2016-11-04"), 571)
final_2018 <- seq(ymd("2017-04-15"), ymd("2018-11-06"), "days")

# create vectors for 2020 cycle
begin_2020 <- rep(ymd("2018-11-07"), 582)
final_2020 <- seq(ymd("2019-04-01"), ymd("2020-11-02"), "days")

# create combined beginning & final vectors
begin <- c(rep(begin_2018, 5), rep(begin_2020, 5))
final <- c(rep(final_2018, 5), rep(final_2020, 5))

# clean up environment
rm(begin_2018, begin_2020, final_2018, final_2020, path)

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

#################### WEIGHT UPDATE FUNCTIONS ####################

# function to update the weight for exponential decay by date function
update_date_weight <- function() {
  
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
  
  # summarise results based on best rmse
  weight_summary <- 
    weight_map %>%
    summarise_weights("date_weight")
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
}

# function to update the weight based on sample size
update_sample_weight <- function() {
  
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
  
  # summarise results based on best rmse
  weight_summary <- 
    weight_map %>%
    summarise_weights("sample_size")
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
}

# function to update the weight based on an individual pollster
update_pollster_weight <- function(pollster) {
  
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
  
  # summarise results based on best rmse
  weight_summary <-
    weight_map %>%
    summarise_weights(pollster)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
}

# function to update a pollster's offset
update_pollster_offset <- function(pollster) {
  
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
  
  # summarise results based on best rmse
  weight_summary <- 
    weight_map %>%
    summarise_weights(offset)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
}

# function to update weight based on population type
update_population_weight <- function(population) {
  
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
  
  # summarise results based on best rmse
  weight_summary <- 
    weight_map %>%
    summarise_weights(population)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
}

# function to update weight based on methodology
update_methodology_weight <- function(methodology) {
  
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
  
  # summarise results based on best rmse
  weight_summary <-
    weight_map %>%
    summarise_weights(methodology)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
}


##################### TESTING ZONE DAWG #######################


variable_weights %>%
  filter(variable != "Ipsos") %>%
  bind_rows(tibble(variable = "Ipsos",
                   weight = 0.75,
                   next_lower = 0,
                   next_upper = 0)) %>%
  pull_pollster_weights()

pull_try_weight("Live Phone", 0.75, "methodology")


plan(multisession, workers = 8)

tictoc::tic()
ipsos_test <- update_pollster_weight("Ipsos")
tictoc::toc()

tictoc::tic()
date_test <- 
  list(begin = begin,
     final = final) %>%
  future_pmap_dfr(~generic_ballot_average(..1,
                                          ..2,
                                          pull_pollster_weights(variable_weights),
                                          pull_sample_weight(),
                                          pull_population_weights(variable_weights),
                                          pull_methodology_weights(variable_weights),
                                          0.75))
tictoc::toc()

date_test %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = ci_lower,
                  ymax = ci_upper),
              alpha = 0.2) +
  geom_line(aes(y = dem2pv),
            size = 1.2) +
  geom_point(data = generic_polls,
             mapping = aes(x = end_date,
                           y = dem2pv),
             alpha = 0.25,
             size = 2)



ipsos_test %>%
  left_join(generic_trend, by = "date") %>%
  select(-starts_with("ci")) %>%
  rename(est = dem2pv.x,
         act = dem2pv.y) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = est,
                color = as.character(weight))) +
  geom_line(aes(y = act),
            size = 1.1)


# initialize rmse tracker - get baseline predictions
baseline <- 
  list(begin = begin,
       final = final) %>%
  pmap_dfr(~generic_ballot_average(..1,
                                   ..2,
                                   pull_pollster_weights(variable_weights),
                                   pull_sample_weight(),
                                   pull_population_weights(variable_weights),
                                   pull_methodology_weights(variable_weights),
                                   pull_date_weight()))

# initialize rmse tracker - build baseline tibble
rmse_tracker <- 
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



test <- 
  test %>% 
  summarise_weights("date_weight")



test %>% update_rmse_tracker()

rmse_tracker

test_2020 <- 
  list(begin = begin_2020,
       final = final_2020) %>%
  pmap_dfr(~generic_ballot_average(..1,
                                   ..2,
                                   pull_pollster_weights(variable_weights),
                                   pull_sample_weight(),
                                   pull_population_weights(variable_weights),
                                   pull_methodology_weights(variable_weights),
                                   pull_date_weight()))

test_2020 %>%
  ggplot(aes(x = date, y = dem2pv)) +
  geom_line()
  

read_csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_polllist.csv") %>%
  distinct(url)







