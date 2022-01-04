# libraries ----
library(tidyverse)
library(lubridate)
library(tidycensus)
library(riekelib)
library(furrr)

# clean up environment ----
rm(list = ls())

# themes ----
source("https://raw.githubusercontent.com/markjrieke/thedatadiary/main/dd_theme_elements/dd_theme_elements.R")
theme_set(theme_minimal(base_family = "Roboto Slab"))

# setup parallel processing (Windows) ----
num_cores <- parallel::detectCores()
clusters <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(clusters)

# wrangle house_polls ----

# read in data
house_polls <-
  read_csv("data/polls/src/fte/house_polls_historical.csv")

# initial col selection & light wrangle
house_polls <- 
  house_polls %>%
  select(question_id, 
         cycle, 
         state, 
         end_date, 
         display_name, 
         sample_size, 
         population_full,
         methodology, 
         seat_name, 
         candidate_name, 
         candidate_party, 
         pct) %>%
  rename(pollster = display_name,
         population = population_full,
         seat = seat_name) %>%
  mutate(methodology = replace_na(methodology, "Unknown"),
         pct = pct/100,
         end_date = mdy(end_date)) %>%
  filter(!is.na(sample_size),
         state != "Puerto Rico") %>%
  mutate(seat = paste(state, seat)) 

# get list of top candidates from each party
# this will let us use the two-party voteshare for major candidates
#
# should note that if two candidates have the same name, or if a candidate loses
# one year then runs another year in a different election, this might cause some 
# issues, but a quick check shows that it doesn't for this training set. 
candidates <- 
  house_polls %>%
  group_by(question_id) %>%
  slice_max(pct, n = 2) %>%
  ungroup() %>%
  filter(candidate_party != "REF") %>%
  bind_cols(counter = 1) %>%
  group_by(question_id, candidate_party) %>%
  slice_max(pct, n = 1) %>%
  ungroup() %>%
  distinct(candidate_name) %>%
  pull(candidate_name)

# wrangle polls to wide format
house_polls <-
  house_polls %>%
  filter(candidate_name %in% candidates) %>%
  #mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
  #       methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  pivot_wider(names_from = candidate_party,
              values_from = c(pct, candidate_name)) %>%
  mutate(across(starts_with("candidate"), ~replace_na(.x, "Unopposed")),
         across(starts_with("pct"), ~replace_na(.x, 0))) %>%
  select(-question_id) %>%
  relocate(starts_with("candidate"), .after = state) 
  
# pull in national polls
national_polls <-
  read_csv("data/polls/src/fte/generic_ballot_polls_historical.csv")

# wrangle national polls
national_polls <- 
  national_polls %>%
  select(cycle, 
         end_date,
         pollster = display_name,
         sample_size,
         population = population_full,
         methodology,
         pct_DEM = dem,
         pct_REP = rep) %>%
  mutate(state = "United States",
         candidate_name_DEM = "Generic DEM",
         candidate_name_REP = "Generic REP",
         end_date = mdy(end_date),
         seat = "United States",
         pct_DEM = pct_DEM/100,
         pct_REP = pct_REP/100) %>%
  select(cycle,
         state,
         candidate_name_DEM,
         candidate_name_REP,
         end_date,
         pollster,
         sample_size,
         population,
         methodology,
         seat,
         pct_DEM,
         pct_REP)

# merge all house polls together
house_polls <- 
  bind_rows(
    house_polls,
    national_polls
  )

# clean up environment
rm(national_polls)

# mend nas
house_polls <- 
  house_polls %>%
  mutate(methodology = replace_na(methodology, "Unknown")) %>%
  drop_na()

# get list of pollsters who have conducted at least 1% of polls
pollsters <- 
  house_polls %>%
  percent(pollster) %>%
  arrange(desc(pct)) %>%
  filter(pct > 0.01) %>%
  pull(pollster)

# get list of methodologies that makeup at least 1% of polls
methods <- 
  house_polls %>%
  percent(methodology) %>%
  arrange(desc(pct)) %>%
  filter(pct > 0.01) %>%
  pull(methodology)

# replace pollsters/methodologies that aren't used regularly with "Other", mutate to dem2pv
house_polls <- 
  house_polls %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  mutate(dem2pv = pct_DEM/(pct_DEM + pct_REP)) %>%
  select(-starts_with("pct"))

# wrangle district demographic data ----

# create lists of demographic vars
census_codes <- 
  tribble(
    ~survey, ~code, ~race,
    "pl", "P2_001N", "total",
    "pl", "P2_005N", "white",
    "pl", "P2_006N", "black",
    "pl", "P2_002N", "hispanic",
    "pl", "P2_008N", "asian",
    "pl", "P2_009N", "pac_islander",
    "acs", "B03001_001", "total",
    "acs", "B03002_003", "white",
    "acs", "B03002_004", "black",
    "acs", "B03001_003", "hispanic",
    "acs", "B03002_006", "asian",
    "acs", "B03002_007", "pac_islander"
  )
  
demo_pl <-
  census_codes %>%
  filter(survey == "pl") %>%
  pull(code)

demo_acs <- 
  census_codes %>%
  filter(survey == "acs") %>%
  pull(code)

# pull demographic data from census api
demographics <- 
  bind_rows(
    get_decennial("us", demo_pl, year = 2020) %>% mutate(year = 2020) %>% rename(estimate = value),
    get_decennial("congressional district", demo_pl, year = 2020) %>% mutate(year = 2020) %>% rename(estimate = value),
    get_acs("us", demo_acs, year = 2019) %>% mutate(year = 2019),
    get_acs("congressional district", demo_acs, year = 2019) %>% mutate(year = 2019),
    get_acs("us", demo_acs, year = 2018) %>% mutate(year = 2018),
    get_acs("congressional district", demo_acs, year = 2018) %>% mutate(year = 2018)
  )

# summarise demographic data
demographics <- 
  demographics %>%
  left_join(census_codes, by = c("variable" = "code")) %>%
  select(year,
         region = NAME,
         race,
         estimate) %>%
  pivot_wider(names_from = race,
              values_from = estimate) %>%
  mutate(across(white:pac_islander, ~ .x/total),
         aapi = asian + pac_islander,
         other = 1 - white - black - hispanic - aapi) %>%
  select(-total, -asian, -pac_islander) %>%
  mutate(region = str_remove_all(region, " \\(116th Congress\\)")) %>%
  arrange(region) %>%
  filter(!str_detect(region, "not defined"),
         !str_detect(region, "District of Columbia"),
         !str_detect(region, "Puerto Rico"))

# recode region names
district_recodes <- read_csv("data/models/house_ballot/district_recodes.csv")

demographics <- 
  demographics %>%
  left_join(district_recodes, by = "region") %>%
  mutate(region = recode) %>%
  select(-recode)

# determine similarity scores ---

# function for calculating similarity scores given one congressional district
# note that this assumes a gaussian kernel for the similarity
similarity <- function(.data, district) {
  
  .data %>%
    mutate(comparison = district) %>%
    left_join(.data, by = c("comparison" = "region")) %>%
    select(-year.y) %>%
    rename(year = year.x) %>%
    mutate(across(ends_with(".x"), expit),
           across(ends_with(".y"), expit),
           white_similar = exp(-((white.y - white.x)^2)/sd(white.x)),
           black_similar = exp(-((black.y - black.x)^2)/sd(black.x)),
           hispanic_similar = exp(-((hispanic.y - hispanic.x)^2)/sd(hispanic.x)),
           aapi_similar = exp(-((aapi.y - aapi.x)^2)/sd(aapi.x)),
           other_similar = exp(-((other.y - other.x)^2)/sd(other.x)),
           similarity = white_similar * black_similar * hispanic_similar * aapi_similar * other_similar) %>%
    select(year, region, comparison, similarity) 
  
}

# function for calculating similarity scores for all congressional districts in a given year
similarities <- function(end_year) {
  
  # temporary df of selected year
  df <- 
    demographics %>%
    filter(year == end_year)
  
  # congressional districts in that year
  districts <- 
    df %>%
    distinct(region) %>%
    pull(region)
  
  message(paste("Mapping data for", end_year))
  
  # map similarity function to congressional district
  districts %>%
    future_map_dfr(~similarity(df, .x))
  
}

# setup multisession workers
plan(multisession, workers = 8)

# create a similariteis tibble
district_similarities <- 
  bind_rows(
    similarities(2018),
    similarities(2019),
    similarities(2020)
  )

#################### FUNCTION DEFINITIONS ####################

# return a tibble with a target district's similarity score to each poll
target_district <- function(target, target_cycle) {
  
  # get to just that cycle's polls
  target_polls <-
    house_polls %>%
    filter(cycle == target_cycle)
  
  # get to just that target/cycle's comparison list
  target_similarities <-
    district_similarities %>%
    filter(region == target,
           year == target_cycle)
  
  # return the polls for that cycle augmented w/the target region's similarity score
  target_polls <- 
    target_polls %>%
    left_join(target_similarities, by = c("seat" = "comparison")) %>%
    select(-year, -region)
  
  return(target_polls)
  
}

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
    tibble(variable = c("rv", "lv", "a", "v"),
           weight = 1,
           next_lower = 0,
           next_upper = 1),
    
    # methodology weights
    tibble(variable = c(methods, "Other Method"),
           weight = 1,
           next_lower = 0,
           next_upper = 1),
    
    # similarity weight
    tibble(variable = "similarity_weight",
           weight = 1,
           next_lower = 0,
           next_upper = 1)
    
  ) %>%
    
    # initialize with "not final" as the search suggestion for each variable
    bind_cols(search_suggestion = "not final")
  
}

# construct a tibble for pollster weights and offsets
pull_pollster_weights <- function() {
  
  variable_weights %>%
    filter(str_detect(variable, " Offset")) %>%
    select(variable, weight) %>%
    rename(offset = weight) %>%
    mutate(variable = str_remove(variable, " Offset")) %>%
    left_join(variable_weights, by = "variable") %>%
    select(-starts_with("next"), -search_suggestion) %>%
    rename(pollster = variable,
           pollster_offset = offset,
           pollster_weight = weight)
  
}

# pull sample_size weight
pull_sample_weight <- function() {
  
  variable_weights %>%
    filter(variable == "sample_size") %>%
    pull(weight)
  
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

#################### TESTING ZONG MY GUY ####################

target_district("Texas District 19", 2020)

pull_date_weight()

variable_weights <- initialize_weights()

house_average <- function(.data,
                          final_date,
                          pollster_weight,
                          sample_weight,
                          population_weight,
                          method_weight,
                          similarity_weight,
                          downweight = 1) {
  
  .data %>%
    
    
}

variable_weights <- initialize_weights()

#################### notes ####################
  
# uncertain:    p < 0.65
# likely:       p < 0.85
# very likely:  p < 0.99
# safe:         p >= 0.99

# maybe think about removing the 2019 cycle...


