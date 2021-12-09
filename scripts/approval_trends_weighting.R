# libraries ----
library(tidyverse)
library(lubridate)
library(furrr)
library(riekelib)
library(patchwork)

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
         methodology = replace_na(methodology, "Unknown")) %>%
  drop_na() %>%
  rename(pollster = display_name)

# separate approval & disapproval trends
approval_trends <- 
  approval_trends %>%
  filter(subgroup == "All polls") %>%
  select(modeldate, approve_estimate, disapprove_estimate) %>%
  mutate(modeldate = mdy(modeldate)) %>%
  rename(date = modeldate)

disapproval_trends <-
  approval_trends %>%
  select(date, disapprove_estimate)

approval_trends <-
  approval_trends %>%
  select(date, approve_estimate)

# create a list of pollsters who have conducted at least 5 approval polls
pollsters <- 
  approval_polls %>%
  count(pollster) %>%
  arrange(desc(n)) %>%
  filter(n >= 5) %>%
  pull(pollster)

# create a list of methodologies that have been used at least 5 times
# this ends up being *all* of them, but we'll need this list for ensuring that
# new methodologies are in this list
methods <- 
  approval_polls %>%
  count(methodology) %>%
  filter(n >= 5) %>%
  arrange(desc(n)) %>%
  pull(methodology)

# replace pollsters/methodologies that don't occur often with "Other"
approval_polls <- 
  approval_polls %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  drop_na()

# create vectors for the approval timeframe
begin <- rep(ymd("2017-01-22"), 1459)
final <- seq(ymd("2017-01-23"), ymd("2021-01-20"), "days")



#################### TESTING ZONE DAWG ####################

begin_date <- ymd("2017-01-22")
final_date <- ymd("2021-01-20")

approval_average <- function(.data,
                             begin_date,
                             final_date, 
                             pollster_weight,
                             sample_weight,
                             population_weight,
                             method_weight,
                             date_weight) {
  
  approval_polls %>%
    
    # filter to just the relevant dates
    filter(end_date <= final_date,
           end_date >= begin_date) %>%
    
    
}

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
generic_ballot_average <- function(.data,
                                   begin_date,
                                   final_date,
                                   pollster_weight,
                                   sample_weight,
                                   population_weight,
                                   method_weight,
                                   date_weight) {
  
  .data %>%
    
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









