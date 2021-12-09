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












