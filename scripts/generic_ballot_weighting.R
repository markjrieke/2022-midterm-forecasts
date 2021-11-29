# libraries ----
library(tidyverse)
library(lubridate)
library(furrr)
library(riekelib)

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
  )

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

# function writing! ----

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

# construct a tibble for pollster weights and offsets
pull_pollster_weights <- function() {
  
  variable_weights %>%
    filter(str_detect(variable, " Offset")) %>%
    select(variable, weight) %>%
    rename(offset = weight) %>%
    mutate(variable = str_remove(variable, " Offset")) %>%
    left_join(variable_weights, by = "variable") %>%
    select(-starts_with("next")) %>%
    rename(pollster = variable,
           pollster_offset = offset,
           pollster_weight = weight)
  
} 
  
# construct a tibble for weights by survey population
pull_population_weights <- function() {
  
  variable_weights %>%
    filter(variable %in% c("rv", "lv", "a", "v")) %>%
    select(variable, weight) %>%
    rename(population_full = variable,
           population_weight = weight)
  
} 
  
# construct a tibble for weights by survey methodology
pull_methodology_weights <- function() {
  
  variable_weights %>%
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

# create sequence of new weights to try
sequence_weights <- function(lower_bound, upper_bound) {
  
  try_weight <- seq(lower_bound, upper_bound, length.out = 5)
  try_weight_2018 <- try_weight %>% rep(571) %>% as_tibble() %>% arrange(value) %>% pull(value)
  try_weight_2020 <- try_weight %>% rep(582) %>% as_tibble() %>% arrange(value) %>% pull(value)
  
  weights <- c(try_weight_2018, try_weight_2020)
  
  return(weights)
  
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

# function to update the weight for exponentional decay by date function
update_date_weight <- function() {
  
  # generate lower & upper bounds
  lower_bound <- 
    variable_weights %>%
    filter(variable == "date_weight") %>%
    pull(next_lower)
  
  upper_bound <-
    variable_weights %>%
    filter(variable == "date_weight") %>%
    pull(next_upper)
  
  # create sequence of new weights to try
  weights <- sequence_weights(lower_bound, upper_bound)
  
  # create a list of vectors to map against
  try_list <-
    list(weights = weights,
         begin = begin,
         final = final)
  
  # map inputs to generic_ballot_average
  weight_map <-
    try_list %>%
    future_pmap_dfr(~generic_ballot_average(..2,
                                            ..3,
                                            pull_pollster_weights(),
                                            pull_sample_weight(),
                                            pull_population_weights(),
                                            pull_methodology_weights(),
                                            ..1)) %>%
    bind_cols(weight = weights)
  
  # summarise results based on best rmse
  weight_summary <- 
    weight_map %>%
    summarise_weights("date_weight")
  
  return(weight_summary)
  
}

# function to update the weight based on sample size
update_sample_weight <- function() {
  
  # generate lower & upper bounds
  lower_bound <- 
    variable_weights %>%
    filter(variable == "sample_size") %>%
    pull(next_lower)
  
  upper_bound <-
    variable_weights %>%
    filter(variable == "sample_size") %>%
    pull(next_upper)
  
  # create sequence of new weights to try
  weights <- sequence_weights(lower_bound, upper_bound)
  
  # create a list of vectors to map against
  try_list <-
    list(weights = weights,
         begin = begin,
         final = final) 
  
  # map inputs to generic_ballot_average
  weight_map <-
    try_list %>%
    future_pmap_dfr(~generic_ballot_average(..2,
                                            ..3,
                                            pull_pollster_weights(),
                                            ..1,
                                            pull_population_weights(),
                                            pull_methodology_weights(),
                                            pull_date_weight())) %>%
    bind_cols(weight = weights)
  
  # summarise results based on best rmse
  weight_summary <- 
    weight_map %>%
    summarise_weights("sample_size")
  
  return(weight_summary)
  
}

##################### EVERYTHING BELOW HERE IS BUNK #######################

plan(multisession, workers = 8)
plan(sequential)

tictoc::tic()
test <- update_date_weight()
tictoc::toc()


test %>%
  select(-starts_with("ci")) %>%
  filter(weight != 0,
         date == "2018-11-06") %>%
  ggplot(aes(x = date,
             y = dem2pv,
             color = as.character(weight))) +
  geom_point()



# map a set of 5 weights to the weight_date_mapper function
weight_date <- function(lower_bound, upper_bound) {
  
  
  # create list of vectors to map against
  try_list <-
    list(weights = c(try_weight_2018, try_weight_2020),
         begin = c(rep(begin_2018, 5), rep(begin_2020, 5)),
         final = c(rep(final_2018, 5), rep(final_2020, 5)))
  
  # map inputs to weight_date_mapper, return tibble with date, weight, & estimated dem2pv
  weight_map <- 
    try_list %>%
    pmap_dfr(~weight_date_mapper(..2, ..3, ..1))
  
  # create tibble of rmse for each weight evaluated
  weight_metrics <-
    weight_map %>%  
    
    # join fte trendline
    left_join(generic_trend, by = "date") %>%
    drop_na() %>%
    rename(est = dem2pv.x,
           act = dem2pv.y) %>%
    select(-date) %>%
    
    # calculated each weights rmse
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
  
  # assign new lower & upper bounds
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

  # add to rmse_tracker df
  iteration <- 
    rmse_tracker %>%
    filter(iteration == max(iteration)) %>%
    pull(iteration)
  
  rmse_tracker <-
    bind_rows(rmse_tracker,
              tibble(iteration = iteration + 1,
                     metric = "date_weight",
                     weight = best_weight,
                     rmse = best_rmse,
                     next_lower = next_lower,
                     next_upper = next_upper))
  
}

test








  









