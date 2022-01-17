# libraries ----
library(tidyverse)
library(lubridate)
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

# wrangle polls ----

# read in data
polls <- 
  bind_rows(read_csv("data/polls/src/fte/house_polls_historical.csv") %>% mutate(race = "House"),
            read_csv("data/polls/src/fte/senate_polls_historical.csv") %>% mutate(race = "Senate"), 
            read_csv("data/polls/src/fte/governor_polls_historical.csv") %>% mutate(race = "Governor"))

# initial col selection & renaming
polls <- 
  polls %>%
  select(question_id,
         cycle,
         race,
         state,
         end_date,
         pollster = display_name,
         sample_size,
         population = population_full,
         methodology,
         seat = seat_name,
         candidate_name,
         candidate_party,
         pct) %>%
  
  # impute median sample size for NA
  mutate(sample_size = if_else(is.na(sample_size), median(.$sample_size, na.rm = TRUE), sample_size)) %>%
  
  # replace nas
  mutate(methodology = replace_na(methodology, "Unknown"),
         population = replace_na(population, "Unknown")) %>%
  
  # remedy seats, remove puerto rico
  mutate(seat = if_else(race == "Governor", "Governor", seat),
         seat = paste(state, seat)) %>%
  filter(state != "Puerto Rico") %>%
  
  # col type fix
  mutate(pct = pct/100,
         end_date = mdy(end_date)) %>%
  
  # fix specific candidate parties
  mutate(candidate_party = if_else(candidate_name %in% c("Angus S. King Jr.", "Ricky Dale Harrington", "Bernard Sanders"),
                                   "DEM",
                                   candidate_party)) %>%
  
  # fix split ticket issue w/GA Special election
  filter(!candidate_name %in% c("Matt Lieberman", "Doug Collins")) %>%
  
  # select only top DEM & REP candidates from each poll
  filter(candidate_party %in% c("DEM", "REP")) %>%
  group_by(question_id, candidate_party) %>%
  slice_max(pct, n = 1) %>%
  ungroup() %>%
    
  # if poll was between two D's/two R's, toss
  group_by(question_id) %>%
  mutate(n = 1,
         n_parties = sum(n)) %>%
  ungroup() %>%
  filter(n_parties != 1) %>%
  select(-n, -n_parties) %>%
    
  # reformat frame to wide
  pivot_wider(names_from = candidate_party,
              values_from = c(pct, candidate_name)) %>%
  select(-question_id) %>%
  select(cycle,
         race,
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
  mutate(race = "House",
         state = "United States",
         candidate_name_DEM = "Generic DEM",
         candidate_name_REP = "Generic REP",
         end_date = mdy(end_date),
         seat = "United States",
         pct_DEM = pct_DEM/100,
         pct_REP = pct_REP/100,
         methodology = replace_na(methodology, "Unknown"),
         sample_size = if_else(is.na(sample_size), median(.$sample_size, na.rm = TRUE), sample_size)) %>%
  select(cycle,
         race,
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
polls <- 
  bind_rows(
    polls,
    national_polls
  )

# clean up environment
rm(national_polls)

# get list of pollsters who have conducted at least 1% of polls
pollsters <- 
  polls %>%
  percent(pollster) %>%
  arrange(desc(pct)) %>%
  filter(pct > 0.01) %>%
  pull(pollster)

# get list of methodologies that makeup at least 1% of polls
methods <- 
  polls %>%
  percent(methodology) %>%
  arrange(desc(pct)) %>%
  filter(pct > 0.01) %>%
  pull(methodology)

# replace pollsters/methodologies that aren't used regularly with "Other", mutate to dem2pv
polls <- 
  polls %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  mutate(dem2pv = pct_DEM/(pct_DEM + pct_REP)) %>%
  select(-starts_with("pct"))

# wrangle district demographic data ----

# set to FALSE to pull in information from tidycensus
completed <- TRUE 

if (completed == FALSE) {
  
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
      tidycensus::get_decennial("us", demo_pl, year = 2020) %>% mutate(year = 2020) %>% rename(estimate = value),
      tidycensus::get_decennial("state", demo_pl, year = 2020) %>% mutate(year = 2020) %>% rename(estimate = value),
      tidycensus::get_decennial("congressional district", demo_pl, year = 2020) %>% mutate(year = 2020) %>% rename(estimate = value),
      tidycensus::get_acs("us", demo_acs, year = 2019) %>% mutate(year = 2019),
      tidycensus::get_acs("state", demo_acs, year = 2019) %>% mutate(year = 2019),
      tidycensus::get_acs("congressional district", demo_acs, year = 2019) %>% mutate(year = 2019),
      tidycensus::get_acs("us", demo_acs, year = 2018) %>% mutate(year = 2018),
      tidycensus::get_acs("state", demo_acs, year = 2018) %>% mutate(year = 2018),
      tidycensus::get_acs("congressional district", demo_acs, year = 2018) %>% mutate(year = 2018)
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
  district_recodes <- read_csv("data/models/midterm_model/district_recodes.csv")
  
  demographics <- 
    demographics %>%
    left_join(district_recodes, by = "region") %>%
    mutate(region = recode) %>%
    select(-recode)
  
  # save demographics tibble
  demographics %>%
    write_csv("data/models/midterm_model/demographics.csv")
  
} else {
  
  # if tidycensus data already pulled, just read in the csv
  demographics <-
    read_csv("data/models/midterm_model/demographics.csv")
  
}

# determine similarity scores ---

# set to FALSE to rerun similarity score tibble creation
completed <- TRUE

if (completed == FALSE) {
  
  # function for calculating similarity scores given one congressional district
  # note that this assumes a gaussian kernel for the similarity
  similarity <- function(.data, input_region) {
    
    .data %>%
      mutate(comparison = input_region) %>%
      left_join(.data, by = c("comparison" = "region")) %>%
      select(-year.y) %>%
      rename(year = year.x) %>%
      mutate(across(ends_with(".x"), logit),
             across(ends_with(".y"), logit),
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
    
    # regions in that year
    regions <- 
      df %>%
      distinct(region) %>%
      pull(region)
    
    message(paste("Mapping data for", end_year))
    
    # map similarity function to congressional district
    regions %>%
      future_map_dfr(~similarity(df, .x))
    
  }
  
  # setup multisession workers
  plan(multisession, workers = 8)
  
  # create a similarities tibble
  region_similarities <- 
    bind_rows(
      similarities(2018),
      similarities(2019),
      similarities(2020)
    )
  
  # save similarities tibble
  region_similarities %>%
    write_csv("data/models/midterm_model/region_similarities.csv")
  
} else {
  
  # if similarities data already created, just read in the csv
  region_similarities <-
    read_csv("data/models/midterm_model/region_similarities.csv")
  
}

# pull in historical results to build ballot average
historical_results <- 
  read_csv("data/models/midterm_model/historical_results.csv") %>%
  
  # remove uncontested races/2019
  filter(candidate_name_DEM != "-",
         candidate_name_REP != "-",
         cycle != 2019) %>%
  
  # reformat frame w/dem2pv
  mutate(region = paste(state, seat),
         dem2pv = dem_votes/(dem_votes + rep_votes)) %>%
  select(cycle, region, dem2pv)

#################### HOUSE POLL AGGREGATOR FUNCTION ####################

poll_average <- function(.data,
                         begin_date,
                         final_date,
                         pollster_weight,
                         sample_weight,
                         population_weight,
                         method_weight,
                         similarity_weight,
                         infer_weight,
                         date_weight,
                         downweight = 1) {
  
  .data %>%
    
    # filter to just needed polls
    filter(end_date <= final_date,
           end_date >= begin_date) %>%
    
    # apply pollster weights and offsets
    left_join(pollster_weight, by = "pollster") %>%
    mutate(dem2pv = dem2pv + pollster_offset,
           dem_votes = round(dem2pv * sample_size),
           rep_votes = round((1-dem2pv) * sample_size)) %>%
    select(-pollster_offset) %>%
    
    # apply sample size weight
    mutate(sample_weight = log10(sample_size) * sample_weight) %>%
    
    # apply population weight
    left_join(population_weight, by = "population") %>%
    
    # apply methodology weight
    left_join(method_weight, by = "methodology") %>%
    
    # apply similarity weight
    mutate(similarity = similarity ^ similarity_weight) %>%
    
    # apply infer weight 
    left_join(infer_weight, by = "infer_to_from") %>%
    select(-infer_to_from) %>%
    
    # apply date weight
    mutate(days_diff = as.numeric(final_date - end_date) + 1,
           weeks_diff = days_diff/7,
           date_weight = date_weight ^ weeks_diff) %>%
    select(-days_diff, -weeks_diff) %>%
    
    # create individual poll weights
    mutate(alpha = dem_votes * pollster_weight * sample_weight * population_weight * method_weight * similarity * infer_to_from_weight * date_weight * downweight,
           beta = rep_votes * pollster_weight * sample_weight * population_weight * method_weight * similarity * infer_to_from_weight * date_weight * downweight) %>%
    
    # summarise with a weak uniform prior
    summarise(alpha = sum(alpha) + 1,
              beta = sum(beta) + 1) %>%
    mutate(dem2pv = alpha/(alpha + beta),
           date = final_date) %>%
    beta_interval(alpha, beta) %>%
    select(date, dem2pv, ci_lower, ci_upper)
    
}

# return a tibble with a target district's similarity score to each poll
target_region <- function(target_race, target, target_cycle) {
  
  # get to just that cycle's polls
  target_polls <-
    polls %>%
    filter(cycle == target_cycle)
  
  # get to just that target/cycle's comparison list
  target_similarities <-
    region_similarities %>%
    filter(region == target,
           year == target_cycle)
  
  # return the polls for that cycle augmented w/the target region's similarity score, 
  # as well as the to_from col
  target_polls <- 
    target_polls %>%
    mutate(infer_to_from = paste(target_race, race, sep = "-"),
           comparison = if_else(race == "House", seat, state)) %>%
    left_join(target_similarities, by = "comparison") %>%
    select(-year, -region)
  
  return(target_polls)
  
}

#################### PULL FUNCTIONS ####################

# construct a tibble for pollster weights and offsets
pull_pollster_weights <- function(.data) {
  
  .data %>%
    filter(str_detect(variable, " Offset")) %>%
    select(variable, weight) %>%
    rename(offset = weight) %>%
    mutate(variable = str_remove(variable, " Offset")) %>%
    left_join(.data, by = "variable") %>%
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
pull_population_weights <- function(.data) {
  
  .data %>%
    filter(variable %in% c("rv", "lv", "a", "v")) %>%
    select(variable, weight) %>%
    rename(population = variable,
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

# pull similarity weight
pull_similarity_weight <- function() {
  
  variable_weights %>%
    filter(variable == "similarity_weight") %>%
    pull(weight)
  
}

# pull infer_to_from weight
pull_infer_weights <- function(.data) {
  
  .data %>%
    filter(str_detect(variable, "-")) %>%
    select(variable, weight) %>%
    rename(infer_to_from = variable,
           infer_to_from_weight = weight)
  
}

# util function to create a temporary tibble replacing the current weight with a "try" weight
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
    
  } else if (type == "methodology") {
    
    pulled_tibble <- new_weight_tibble %>% pull_methodology_weights()
    
  } else {
    
    pulled_tibble <- new_weight_tibble %>% pull_infer_weights()
    
  }
  
  return(pulled_tibble)
  
}

# function to pull the next lower or the next upper bound
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

#################### PASSER FUNCTIONS ####################

# pass try_list for date_weight
pass_date_weight <- function(race, cycle, region, begin_date, end_date, date_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date,
                 end_date,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 date_weight)
  
}

# pass try_list for sample_weight
pass_sample_weight <- function(race, cycle, region, begin_date, end_date, sample_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date,
                 end_date,
                 pull_pollster_weights(variable_weights),
                 sample_weight,
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight())
  
}

# pass try_list for similarity_weight
pass_similarity_weight <- function(race, cycle, region, begin_date, end_date, similarity_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 similarity_weight,
                 pull_infer_weights(variable_weights),
                 pull_date_weight())
  
}

# pass try_list for pollster weight
pass_pollster_weight <- function(pollster, race, cycle, region, begin_date, end_date, pollster_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 pull_try_weight(pollster, pollster_weight, "pollster"),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight())
  
}

# pass try_list for pollster offset
pass_pollster_offset <- function(pollster, race, cycle, region, begin_date, end_date, pollster_offset) {
  
  # create pollster offset var
  offset <- paste(pollster, "Offset")
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 pull_try_weight(offset, pollster_offset, "pollster"),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight())
  
}

# pass try_list for population weight
pass_population_weight <- function(population, race, cycle, region, begin_date, end_date, population_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_try_weight(population, population_weight, "population"),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight())
  
}

# pass try_list for methodology weight
pass_methodology_weight <- function(methodology, race, cycle, region, begin_date, end_date, methodology_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_try_weight(methodology, methodology_weight, "methodology"),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights), 
                 pull_date_weight())
  
}

# pass try_list for infer_to_from weight
pass_infer_weight <- function(infer_to_from, race, cycle, region, begin_date, end_date, infer_to_from_weight) {
  
  target_region(race, region, cycle) %>%
    poll_average(begin_date,
                 end_date,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_try_weight(infer_to_from, infer_to_from_weight, "infer_to_from"),
                 pull_date_weight())
  
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
           next_upper = 1),
    
    # add infer_to_from weight
    tibble(variable = c("House-House",
                        "House-Senate",
                        "House-Governor",
                        "Senate-Senate",
                        "Senate-House",
                        "Senate-Governor",
                        "Governor-Governor",
                        "Governor-House",
                        "Governor-Senate"),
           weight = 1,
           next_lower = 0,
           next_upper = 1)
    
  ) %>%
    
    # initialize with "not final" as the search suggestion for each variable
    bind_cols(search_suggestion = "not final")
  
}

# check if the search suggestion is final or ought to continue
check_suggestion <- function(variable_name) {
  
  variable_weights %>%
    filter(variable == variable_name) %>%
    pull(search_suggestion)
  
}

# create a sequence of new weights to try
# (unlike other scripts, only need one sequence - not mapping against days)
sequence_weights <- function(lower_bound, upper_bound) {
  
  seq(lower_bound, upper_bound, length.out = 5)
  
}

# create a vector of weights to be passed to try_list
vectorize_weights <- function(variable_name) {
  
  # generate lower & upper bounds
  lower_bound <-
    pull_bound(variable_name, "lower")
  
  upper_bound <-
    pull_bound(variable_name, "upper")
  
  # create sequence of new weights to try
  weights <- sequence_weights(lower_bound, upper_bound)
  
  return(weights)
  
}

# create list of variables to pass to update functions
create_try_list <- function(variable_name) {
  
  # create vector of cycles
  cycles <- 
    historical_results %>%
    pull(cycle) %>%
    rep(5)
  
  # create vector of districts
  districts <- 
    historical_results %>%
    pull(region) %>%
    rep(5)
  
  # create a vector of begin_dates
  begin_dates <-
    historical_results %>%
    mutate(begin_date = if_else(cycle == 2018, ymd("2016-11-04"), ymd("2018-11-04"))) %>%
    pull(begin_date) %>%
    rep(5)
  
  # create a vector of end_dates
  end_dates <- 
    historical_results %>%
    mutate(end_date = if_else(cycle == 2018, ymd("2018-11-06"), ymd("2020-11-03"))) %>%
    pull(end_date) %>%
    rep(5)
  
  # create vector of weights
  weights <- 
    vectorize_weights(variable_name) %>%
    rep(nrow(historical_results)) %>%
    arrange_vector()
  
  # create try list
  try_list <-
    list(cycle = cycles,
         district = districts,
         begin_date = begin_dates,
         end_date = end_dates,
         weight = weights)
  
  return(try_list)
  
}

# bind actual results against estimates returned by passer functions
bind_results <- function(.data, input_list) {
  
  # filter down to just returned estimate
  .data %>%
    select(dem2pv) %>%
    
    # bind list to new cols
    bind_cols(cycle = input_list$cycle,
              district = input_list$district,
              weight = input_list$weight) %>%
    
    # append with historical results
    left_join(historical_results, by = c("cycle" = "cycle", "district" = "region")) %>%
    select(weight,
           est = dem2pv.x,
           act = dem2pv.y)
  
}

# summarise weights and recommend new bounds
summarise_weights <- function(.data, metric) {
  
  # calculate each weights rmse
  weight_metrics <- 
    .data %>%
    group_by(weight) %>%
    nest() %>%
    mutate(rmse = map(data, yardstick::rmse, truth = act, estimate = est)) %>%
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
  
  # note whether or not to continue to search for better weights, based off difference threshold
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

# function for initializing rmse_tracker tibble
initialize_rmse <- function() {
  
  # append historical results with end_date
  results <-
    historical_results %>%
    mutate(begin_date = if_else(cycle == 2018, ymd("2016-11-04"), ymd("2018-11-04")),
           end_date = if_else(cycle == 2018, ymd("2018-11-06"), ymd("2020-11-03")))
  
  # create try list to pass to passer function
  try_list <-
    list(cycle = results$cycle,
         district = results$region,
         begin_date = results$begin_date,
         end_date = results$end_date,
         weight = rep(1, results %>% nrow()))
  
  # pass try list to passer function
  message("Creating baseline")
  tictoc::tic()
  
  baseline <-
    try_list %>%
    future_pmap_dfr(~pass_date_weight(..1, ..2, ..3, ..4, ..5))
  
  tictoc::toc()
  
  # build baseline tibble
  baseline <- 
    baseline %>%
    bind_results(try_list) %>%
    select(-weight) %>%
    nest(data = everything()) %>%
    mutate(rmse = map(data, yardstick::rmse, truth = act, estimate = est)) %>%
    unnest(rmse) %>%
    select(rmse = .estimate) %>%
    mutate(index = 0,
           metric = "baseline",
           weight = 0,
           next_lower = 0,
           next_upper = 0,
           pct_diff = 0,
           search_suggestion = "baseline") %>%
    relocate(rmse, .after = weight)
  
  return(baseline)
  
}

# function for initializing both variable weights and rmse tracker (ensures called in proper order)
initialize_tables <- function() {
  
  # <<- interacts with global environment
  variable_weights <<- initialize_weights()
  rmse_tracker <<- initialize_rmse()
  
}

# function for adding metrics to running list of rmse
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

# function for updating the variable weight table
update_weight_table <- function(.data, variable_name) {
  
  # <<- interacts with the global object
  variable_weights <<-
    variable_weights %>%
    filter(variable != variable_name) %>%
    
    # reformat & bind .data with new weights/suggestions
    bind_rows(.data %>% select(-rmse, -pct_diff) %>% rename(variable = metric))
  
}

# util function for updating both tables
update_tables <- function(.data, variable_name, input_list) {
  
  # summarise results based on best rmse
  weight_summary <-
    .data %>%
    bind_results(input_list) %>%
    summarise_weights(variable_name)
  
  # update rmse tracker
  weight_summary %>% update_rmse_tracker()
  
  # update weight table
  weight_summary %>% update_weight_table(variable_name)
    
}



#################### TESTING ZONG MY GUY ####################


# update date_weight
update_date_weight <- function(district, cycle) {
  
  # get the end date based on cycle
  end_date <- assign_end_date(cycle)
  
  # do not evaluate if weight is final
  if (check_suggestion("date_weight") == "final") {
    
    message("date_weight marked as final and will not be updated.")
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list("date_weight")
      
    # map inputs to passer function
    try_list %>%
      future_pmap_dfr(~pass_date_weight(..1, ..2, ..3, ..4))
      
      # update rmse & variable weight tables
      
      
  }
    
}

target_region("Ohio District 4", 2020) %>%
  house_average(ymd("2018-11-07"),
                ymd("2020-11-04"),
                pull_pollster_weights(variable_weights),
                pull_sample_weight(),
                pull_population_weights(variable_weights),
                pull_methodology_weights(variable_weights),
                pull_similarity_weight(),
                pull_date_weight())

pass_date_weight(2020, "Ohio District 4", ymd("2018-11-07"), ymd("2020-11-04"), 1)

variable_weights <- initialize_weights()
rmse_tracker <- initialize_rmse()


test_try <- create_try_list("date_weight")

test_weight_map <- 
  test_try %>%
  future_pmap_dfr(~pass_date_weight(..1, ..2, ..3, ..4))

test_weight_map %>%
  bind_results(test_try)

test_weight_map %>%
  select(dem2pv) %>%
  bind_cols(cycle = test_try$cycle,
            district = test_try$district,
            weight = test_try$weight) %>%
  left_join(historical_results, by = c("cycle" = "cycle", "district" = "region")) %>%
  select(weight,
         est = dem2pv.x,
         act = dem2pv.y)

#################### notes ####################

# to-do:
#   add in senate/gubernatorial polls
#     add in similarities based on race data
# 
#   remove dependencies on begin_date (taken care of by target_region)
#
#   add new feature infer_to_from
#     i.e., if you're predicting a senate seat and adding a house poll, add senate_house
#     infer_to_from options
#       senate_house
#       senate_governor
#       senate_senate
#       house_senate
#       house_governor
#       house_house
#       governor_house
#       governor_senate
#       governor_governor
#     update fns() w/this 
#       poll aggregator fn
#       pull fn?
#       initialize weights
#       add passer
#       create_try_list()? (or imputed elsewhere?)
#
#   train mean
#     add update_x_weight() functions
#     add call_x() functions
#     add update_all() function
#
#   train error
#     add update_downweight() function
#   
#   explore results
#     verify that no seat type is significantly off 
#
#   consider adding in off-season elections?
#     2019 NC
#     2021 VA Gov?

# ratings ranges:
#   uncertain:    p < 0.65
#   likely:       p < 0.85
#   very likely:  p < 0.99
#   safe:         p >= 0.99

# some potential concerns to make public:
#   racial makeup (ignore hisp. black, asian diversity, etc.)
#   data leakage w/polls (poll training includes all historical results)
#   similarity scores based only on race
#   only looking at top D/R (e.g., Feinstein & Collins aren't in the model)
#   only looking at general election, not runoff
#   doesn't include common features that definitely have signal (e.g., prez approval)
#     due to lack of historical polling data (only 2 cycle's worth)

# potential features:
#   data currently available in repo:
#     polls
#     polls CI spread
#     polls CI lower
#     polls CI upper
#     number of polls for the race
#     number of polls in the last x wks
#     racial makeup
#     incumbency
#     prev-party
#   data available online:
#     years in seat
#     years in gov't
#     endorsements
#     total fundraising
#     spending ratio (dem/rep & rep/dem)
#     partisan lean (maybe...)
#     experts' ratings (maybe...)
#     candidate demographics (age, race, gender, age... maybe...)
#     candidate idealogy (dw nominate - not sure if this is available for non-incumbents)
#   data maybe available in the future
#     median or mean age
#     population density
#     religious affiliation
#     educational attainment

# possibly add in the senate?
#   one model for house & senate
#   senate polls informed by house polls & vice-versa
#     pull in state demographic info for senate similarities...
#   in xgboost model, stratify initial split by chamber to ensure equal coverage
#   only 66 senate races to pull data from ...
#
#   build out house polls first, then check to see if adding senate polls is any value
#   if senate polls are way off, don't use
#
#   should I also do the same with governors ? 
#   would this be double-dipping on similarities (states for senate)
#
# another thought: there are features I'm not sure I can use for a blended model?
#   basically any non-racial information is unavailable for the districts
#   maybe check in on DRA? Not sure what information is contained there (it's just race)
#   maybe I can create blended & non-blended forecasts & see which performs better
#     Feature-limited for blended forecast, but data-rich
#     Data-limited for non-blended forecast, but feature-rich
#   in either regard, I think I ought to start with a blended poll aggregator
#   my hunch is that the blended forecast will perform better (if nothing else than for the extra data points...)

# maybe think about removing the 2019 cycle...


