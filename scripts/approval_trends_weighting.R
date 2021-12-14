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
  select(modeldate, approve_estimate, disapprove_estimate) %>%
  mutate(modeldate = mdy(modeldate)) %>%
  rename(date = modeldate)

disapproval_trends <-
  approval_trends %>%
  select(date, disapprove_estimate)

approval_trends <-
  approval_trends %>%
  select(date, approve_estimate)

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
begin <- rep(ymd("2017-01-22"), 1459)
final <- seq(ymd("2017-01-23"), ymd("2021-01-20"), "days")

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
                             answer) {
  
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
    mutate(alpha = answer_votes * pollster_weight * sample_weight * population_weight * method_weight * date_weight,
           beta = not_answer_votes * pollster_weight * sample_weight * population_weight * method_weight * date_weight) %>%
    
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
  )
  
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



approval_average(approval_polls, 
                 begin_date, 
                 final_date, 
                 pull_pollster_weights(approval_weights),
                 pull_sample_weight(approval_weights),
                 pull_population_weights(approval_weights),
                 pull_methodology_weights(approval_weights),
                 pull_date_weight(approval_weights),
                 no)
                 
 









