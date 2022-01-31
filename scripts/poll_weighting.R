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
  mutate(methodology = replace_na(methodology, "Unknown Method"),
         population = replace_na(population, "Unknown Population")) %>%
  
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
         methodology = replace_na(methodology, "Unknown Method"),
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
  region_recodes <- read_csv("data/models/midterm_model/region_recodes.csv")
  
  demographics <- 
    demographics %>%
    left_join(region_recodes, by = "region") %>%
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
  select(cycle, race, region, dem2pv)

# inferred list
infer_list <-
  c("House-House", "House-Senate", "House-Governor",
    "Senate-Senate", "Senate-House", "Senate-Governor",
    "Governor-Governor", "Governor-House", "Governor-Senate")

#################### HOUSE POLL AGGREGATOR FUNCTION ####################

poll_average <- function(.data,
                         begin_date,
                         final_date,
                         cycle_input,
                         race_input, 
                         seat_input,
                         pollster_weight,
                         sample_weight,
                         population_weight,
                         method_weight,
                         similarity_weight,
                         infer_weight,
                         date_weight,
                         national_poll_weight,
                         downweight = 1) {
  
  # get the number of polls for the specific race
  n_polls <- 
    .data %>%
    filter(cycle == cycle_input,
           race == race_input,
           seat == seat_input) %>%
    nrow()
  
  # get the polling average
  average <- 
    .data %>%
    
    # filter to just needed polls
    filter(end_date <= final_date,
           end_date >= begin_date) %>%
    
    # apply pollster weights and offsets
    left_join(pollster_weight, by = "pollster") %>%
    mutate(dem2pv = expit(logit(dem2pv) + pollster_offset),
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
    
    # apply national poll weight
    mutate(national_weight = if_else(state == "United States", national_poll_weight, 1)) %>%
    
    # create individual poll weights
    mutate(alpha = dem_votes * pollster_weight * sample_weight * population_weight * method_weight * similarity * infer_to_from_weight * date_weight * national_weight * downweight * (1 + n_polls),
           beta = rep_votes * pollster_weight * sample_weight * population_weight * method_weight * similarity * infer_to_from_weight * date_weight * national_weight * downweight * (1 + n_polls)) %>%
    
    # summarise with a weak uniform prior
    summarise(alpha = sum(alpha) + 1,
              beta = sum(beta) + 1) %>%
    mutate(dem2pv = alpha/(alpha + beta),
           date = final_date) %>%
    beta_interval(alpha, beta) %>%
    select(date, dem2pv, ci_lower, ci_upper)
  
  return(average)
    
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
    filter(variable %in% c("rv", "lv", "a", "v", "Unknown Population")) %>%
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

# pull weight for national polls
pull_national_weight <- function() {
  
  variable_weights %>%
    filter(variable == "national_weight") %>%
    pull(weight)
  
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

# filter out strings in region to pass to passer fns
pass_region <- function(region) {
  
  region %>%
    str_remove(" Governor") %>%
    str_remove(" Class III") %>%
    str_remove(" Class II") %>%
    str_remove(" Class I") 
  
}

# pass try_list for date_weight
pass_date_weight <- function(race, cycle, region, begin_date, end_date, date_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date,
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 date_weight,
                 pull_national_weight())
  
}

# pass try_list for sample_weight
pass_sample_weight <- function(race, cycle, region, begin_date, end_date, sample_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date,
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 sample_weight,
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try_list for similarity_weight
pass_similarity_weight <- function(race, cycle, region, begin_date, end_date, similarity_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 similarity_weight,
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try_list for pollster weight
pass_pollster_weight <- function(pollster, race, cycle, region, begin_date, end_date, pollster_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_try_weight(pollster, pollster_weight, "pollster"),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try_list for pollster offset
pass_pollster_offset <- function(pollster, race, cycle, region, begin_date, end_date, pollster_offset) {
  
  # create pollster offset var
  offset <- paste(pollster, "Offset")
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_try_weight(offset, pollster_offset, "pollster"),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try_list for population weight
pass_population_weight <- function(population, race, cycle, region, begin_date, end_date, population_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_try_weight(population, population_weight, "population"),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try_list for methodology weight
pass_methodology_weight <- function(methodology, race, cycle, region, begin_date, end_date, methodology_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date, 
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_try_weight(methodology, methodology_weight, "methodology"),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights), 
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try_list for infer_to_from weight
pass_infer_weight <- function(infer_to_from, race, cycle, region, begin_date, end_date, infer_to_from_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date,
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_try_weight(infer_to_from, infer_to_from_weight, "infer_to_from"),
                 pull_date_weight(),
                 pull_national_weight())
  
}

# pass try list for national_weight
pass_national_weight <- function(race, cycle, region, begin_date, end_date, national_weight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date,
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 national_weight)
  
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
           next_lower = -1,
           next_upper = 1),
    
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
    tibble(variable = c("rv", "lv", "a", "v", "Unknown Population"),
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
           next_upper = 1),
    
    # add national_weight
    tibble(variable = "national_weight",
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
  
  # create vector of races
  races <-
    historical_results %>%
    pull(race) %>%
    rep(5)
  
  # create vector of cycles
  cycles <- 
    historical_results %>%
    pull(cycle) %>%
    rep(5)
  
  # create vector of regions
  regions <- 
    historical_results %>%
    pull(region) %>%
    rep(5)
  
  # create a vector of begin_dates
  begin_dates <-
    historical_results %>%
    mutate(begin_date = if_else(cycle == 2018, ymd("2016-11-04"), ymd("2018-11-07"))) %>%
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
    list(race = races,
         cycle = cycles,
         region = regions,
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
              race = input_list$race,
              region = input_list$region,
              weight = input_list$weight) %>%
    
    # append with historical results
    left_join(historical_results, by = c("race", "cycle", "region")) %>%
    select(weight,
           est = dem2pv.x,
           act = dem2pv.y,
           cycle,
           race,
           region)
  
}

# summarise weights and recommend new bounds
summarise_weights <- function(.data, metric) {
  
  threshold <- 0.0001
  
  # determine weights' rmse
  weight_metrics <- 
    .data %>%
    
    # join polls - rmse is weighted by the number of polls
    left_join(polls %>% count(cycle, race, seat), 
              by = c("cycle", "race", "region" = "seat")) %>%
    mutate(n = replace_na(n, 0),
           n = n + 1) %>%
    rowwise() %>%
    mutate(rmse = yardstick::rmse_vec(act, est)) %>%
    ungroup() %>%
    group_by(weight) %>%
    summarise(rmse = sum(n * rmse)/sum(n)) %>%
    rowid_to_column()
  
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
  
  # rowid == 2 if best rmse = 0
  # this will ensure that the poll doesn't get tossed out entirely
  if (best_weight == 0 & str_detect(metric, "Offset") == FALSE) {
    
    best_weight <- 
      weight_metrics %>%
      filter(rowid == 2) %>%
      pull(weight)
    
  }
  
  # note whether or not to continue to search for better weights, based off difference threshold
  if (pct_diff < threshold) {
    
    search_suggestion <- "final"
    
  } else {
    
    search_suggestion <- "not final"
    
  }
  
  # set offset to 0 if < 1% difference
  # this'll avoid having a pollster offset of -10% when it's not any different from an offset of 0%
  if (pct_diff < threshold & str_detect(metric, "Offset") == TRUE) {
    
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
    mutate(begin_date = if_else(cycle == 2018, ymd("2016-11-04"), ymd("2018-11-07")),
           end_date = if_else(cycle == 2018, ymd("2018-11-06"), ymd("2020-11-03")))
  
  # create try list to pass to passer function
  try_list <-
    list(race = results$race,
         cycle = results$cycle,
         region = results$region,
         begin_date = results$begin_date,
         end_date = results$end_date,
         weight = rep(1, results %>% nrow()))
  
  # pass try list to passer function
  message("Creating baseline")
  tictoc::tic()
  
  baseline <-
    try_list %>%
    future_pmap_dfr(~pass_date_weight(..1, ..2, ..3, ..4, ..5, ..6))
  
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

#################### UPDATE FUNCTIONS ####################

# update date weight
update_date_weight <- function() {
  
  # do not evaluate if weight is final
  if (check_suggestion("date_weight") == "final") {
    
    message("date_weight marked as final and will not be updated.")
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list("date_weight")
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_date_weight(..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables("date_weight", try_list)
    
  }
  
}

# update sample_weight
update_sample_weight <- function() {
  
  # do not evaluate if weight is final
  if (check_suggestion("sample_size") == "final") {
    
    message("sample_size marked as final and will not be updated.")
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list("sample_size")
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_sample_weight(..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables("sample_size", try_list)
    
  }
  
}

# update similarity_weight
update_similarity_weight <- function() {
  
  # do not evaluate if weight is final
  if (check_suggestion("similarity_weight") == "final") {
    
    message("similarity_weight marked as final and will not be updated.")
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list("similarity_weight")
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_similarity_weight(..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables("similarity_weight", try_list)
    
  }
  
}

# update an individual pollster's weight
update_pollster_weight <- function(pollster) {
  
  # do not evaluate if weight is final
  if (check_suggestion(pollster) == "final") {
    
    message(paste(pollster, "marked as final and will not be updated."))
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list(pollster)
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_pollster_weight(pollster, ..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables(pollster, try_list)
    
  }
  
}

# update an individual pollster's offset
update_pollster_offset <- function(pollster) {
  
  # create offset variable
  offset <- paste(pollster, "Offset")
  
  # do not evaluate if offset is final
  if (check_suggestion(offset) == "final") {
    
    message(paste(pollster, "Offset marked as final and will not be updated."))
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list(offset)
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_pollster_offset(pollster, ..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables(offset, try_list)
    
  }
  
}

# update a population's weight
update_population_weight <- function(population) {
  
  if(check_suggestion(population) == "final") {
    
    message(paste(population, "marked as final and will not be updated."))
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list(population)
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_population_weight(population, ..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables(population, try_list)
    
  }
  
}

# update a methodology's weight
update_methodology_weight <- function(methodology) {
  
  if (check_suggestion(methodology) == "final") {
    
    message(paste(methodology, "marked as final and will not be updated."))
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list(methodology)
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_methodology_weight(methodology, ..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables(methodology, try_list)
    
  }
  
}

# update the inferred weight from other polls
update_infer_weight <- function(infer_to_from) {
  
  if (check_suggestion(infer_to_from) == "final") {
    
    message(paste(infer_to_from, "marked as final and will not be updated."))
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list(infer_to_from) 
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_infer_weight(infer_to_from, ..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables(infer_to_from, try_list)
    
  }
  
}

# update the weight associated w/national polls
update_national_weight <- function() {
  
  # do not evaluate if weight is final
  if (check_suggestion("national_weight") == "final") {
    
    message("national_weight marked as final and will not be updated.")
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list("national_weight")
    
    # map inputs to mapper function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_national_weight(..1, ..2, ..3, ..4, ..5, ..6))
    
    # update tables
    weight_map %>%
      update_tables("national_weight", try_list)
    
  }
  
}

#################### CALLER FUNCTIONS ####################

# util function for messaging user and calling update
call_update_date <- function() {
  
  message("Updating `date_weight`.")
  tictoc::tic()
  update_date_weight()
  tictoc::toc()
  message("`date_weight` updated.")
  message()
  
}

# util function for messaging user and calling update
call_update_sample <- function() {
  
  message("Updating `sample_size`.")
  tictoc::tic()
  update_sample_weight()
  tictoc::toc()
  message("`sample_size` updated.")
  message()
  
}

# util function for messaging user and calling update
call_update_similarity <- function() {
  
  message("Updating `similarity_weight`.")
  tictoc::tic()
  update_similarity_weight()
  tictoc::toc()
  message("`similarity_weight` updated.")
  message()
  
}

# util function for messaging user and calling update
call_update_pollster <- function(pollster) {
  
  message(paste("Updating", pollster, "weight."))
  tictoc::tic()
  update_pollster_weight(pollster)
  tictoc::toc()
  message(paste(pollster, "weight updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_offset <- function(pollster) {
  
  message(paste("Updating", pollster, "Offset."))
  tictoc::tic()
  update_pollster_offset(pollster)
  tictoc::toc()
  message(paste(pollster, "Offset updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_population <- function(population) {
  
  message(paste("Updating", population, "weight."))
  tictoc::tic()
  update_population_weight(population)
  tictoc::toc()
  message(paste(population, "weight updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_methodology <- function(methodology) {
  
  message(paste("Updating", methodology, "weight."))
  tictoc::tic()
  update_methodology_weight(methodology)
  tictoc::toc()
  message(paste(methodology, "weight updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_infer <- function(infer_to_from) {
  
  message(paste("Updating", infer_to_from, "weight."))
  tictoc::tic()
  update_infer_weight(infer_to_from)
  tictoc::toc()
  message(paste(infer_to_from, "weight updated."))
  message()
  
}

# util function for messaging user and calling update
call_update_national <- function() {
  
  message("Updating `national_weight`.")
  tictoc::tic()
  update_national_weight()
  message("`national_weight` updated.")
  tictoc::toc()
  
}

#################### UPDATE ALL FUNCTION ####################

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
    if (response == "Initialize new baseline") {
      
      message("Initializing new baseline for `variable_weights` and `rmse_tracker`.")
      message("This may take a few minutes.")
      message()
      
      # initialize weights rmse tracker
      initialize_tables()
      
    } else {
      
      message("Update aborted.")
      return()
      
    }
    
  } else {
    
    message("Reading in weights and rmse tracker...")
    message()
    
    # <<- interact with global environment
    variable_weights <<- read_csv("data/models/midterm_model/variable_weights.csv")
    rmse_tracker <<- read_csv("data/models/midterm_model/rmse_tracker.csv")
    
  }
  
  # determine the number of updates to be made
  num_updates <-
    variable_weights %>%
    count(search_suggestion) %>%
    filter(search_suggestion == "not final") %>%
    pull(n)
  
  # determine the approximate runtime (~120s per variable, added buffer time)
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
    call_update_date()
    call_update_national()
    infer_list %>% walk(call_update_infer)
    call_update_similarity()
    call_update_sample()
    c(pollsters, "Other Pollster") %>% walk(call_update_pollster)
    c(pollsters, "Other Pollster") %>% walk(call_update_offset)
    c("rv", "lv", "a", "v", "Unknown Population") %>% walk(call_update_population)
    c(methods, "Other Method") %>% walk(call_update_methodology)
    
    # write updates to data/models
    variable_weights %>% write_csv("data/models/midterm_model/variable_weights.csv")
    rmse_tracker %>% write_csv("data/models/midterm_model/rmse_tracker.csv")
    
  }
  
}

#################### VIZ FUNCTIONS ####################

# passer function to get the current fit
pass_current_fit <- function(race, cycle, region, begin_date, end_date) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date,
                 end_date,
                 cycle,
                 race,
                 region,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 pull_national_weight())
  
  
}

# function to get the current fit
get_current_fit <- function() {
  
  # modify historical_results with begin/end date
  results <- 
    historical_results %>%
    mutate(begin_date = if_else(cycle == 2018, ymd("2016-11-04"), ymd("2018-11-07")),
           end_date = if_else(cycle == 2018, ymd("2018-11-06"), ymd("2020-11-03")))
  
  # create try list to pass to passer fn
  try_list <-
    list(race = results %>% pull(race),
         cycle = results %>% pull(cycle),
         region = results %>% pull(region),
         begin_date = results %>% pull(begin_date),
         end_date = results %>% pull(end_date))
  
  # pass try_list to passer fn
  weight_map <-
    try_list %>%
    future_pmap_dfr(~pass_current_fit(..1, ..2, ..3, ..4, ..5))
  
  # bind to results
  current_results <-
    weight_map %>%
    select(dem2pv, starts_with("ci")) %>%
    bind_cols(cycle = try_list$cycle,
              region = try_list$region) %>%
    left_join(historical_results, by = c("cycle", "region")) %>%
    select(cycle, 
           race, 
           region,
           est = dem2pv.x,
           starts_with("ci"),
           act = dem2pv.y)
    
  return(current_results)
  
}

# visualize current fit and error
visualize_current_fit <- function(.data) {
  
  .data %>%
    left_join(polls %>% count(cycle, race, seat),
              by = c("cycle", "race", "region" = "seat")) %>%
    mutate(n = replace_na(n, 0),
           `n + 1` = n + 1) %>%
    ggplot(aes(x = act,
               y = est,
               color = abs(est - act),
               size = `n + 1`)) +
    geom_point(alpha = 0.25) +
    geom_abline(linetype = "dashed",
                color = "gray") +
    scale_color_viridis_c() +
    labs(color = "error") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_size_continuous(range = c(1, 10)) +
    coord_equal()
  
}

# visualize current fit, faceted by race
visualize_facet_fit <- function(.data) {
  
  .data %>%
    visualize_current_fit() +
    facet_wrap(~race)
  
}

# visualize rmse
visualize_rmse <- function(.data) {
  
  .data %>%
    filter(search_suggestion != "baseline") %>%
    ggplot(aes(x = index, 
               y = rmse)) +
    geom_line(size = 1.1,
              color = "midnightblue")
  
}

#################### MODELTIME ####################

# set to FALSE to rerun rounds
completed <- TRUE

if (completed == FALSE) {
  
  # round 0
  initialize_tables()
  variable_weights %>% write_csv("data/models/midterm_model/variable_weights.csv")
  rmse_tracker %>% write_csv("data/models/midterm_model/rmse_tracker.csv")
  
  # round 0 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 1
  update_all()
  
  # round 1 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 2
  update_all()
  
  # round 2 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 3
  update_all()
  
  # round 3 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 4
  update_all()
  
  # round 4 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 5
  update_all()
  
  # round 5 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 6
  update_all()
  
  # round 6 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 7
  update_all()
  
  # round 7 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 8
  update_all()
  
  # round 8 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 9
  update_all()
  
  # round 9 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 10
  update_all()
  
  # round 10 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 11
  update_all()
  
  # round 11 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 12
  update_all()
  
  # round 12 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
  # round 13
  update_all()
  
  # round 13 viz
  current_results <- get_current_fit()
  current_results %>% visualize_current_fit()
  current_results %>% visualize_facet_fit()
  rmse_tracker %>% visualize_rmse()
  
}

#################### EXPLORE FIT ####################

# set to FALSE to rerun
completed <- TRUE

if (completed == FALSE) {
  
  # get current results
  current_results <- get_current_fit()
  
  # save viz's
  current_results %>% visualize_current_fit()
  
  ggsave("plots/midterm_forecast/final_fit.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  current_results %>% visualize_facet_fit()
  
  ggsave("plots/midterm_forecast/final_fit_facet.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  rmse_tracker %>% visualize_rmse()
  
  ggsave("plots/midterm_forecast/rmse_tracker.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  # compare against confidence interval
  current_results %>%
    mutate(correct = if_else(act <= ci_upper & act >= ci_lower, "y", "n")) %>%
    ggplot(aes(x = act,
               y = est,
               ymin = ci_lower,
               ymax = ci_upper,
               color = abs(est - act))) +
    geom_pointrange(size = 0.75,
                    alpha = 0.25) +
    geom_abline(linetype = "dashed",
                color = "gray") +
    scale_color_viridis_c() +
    labs(color = "error") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    coord_equal()
  
  ggsave("plots/midterm_forecast/initial_pointrange.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  current_results %>%
    mutate(correct = if_else(act <= ci_upper & act >= ci_lower, "y", "n")) %>%
    ggplot(aes(x = act,
               y = est,
               ymin = ci_lower,
               ymax = ci_upper,
               color = abs(est - act))) +
    geom_pointrange(size = 0.75,
                    alpha = 0.25) +
    geom_abline(linetype = "dashed",
                color = "gray") +
    scale_color_viridis_c() +
    labs(color = "error") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    coord_equal() +
    facet_wrap(~race)
  
  ggsave("plots/midterm_forecast/initial_pointrange_facet.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  current_results %>%
    mutate(correct = if_else(act <= ci_upper & act >= ci_lower, "y", "n")) %>%
    filter(correct == "y") %>% 
    ggplot(aes(x = act,
               y = est,
               ymin = ci_lower,
               ymax = ci_upper,
               color = abs(est - act))) +
    geom_pointrange(size = 0.75,
                    alpha = 0.25) +
    geom_abline(linetype = "dashed",
                color = "gray") +
    scale_color_viridis_c() +
    labs(color = "error") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    coord_equal()
  
  ggsave("plots/midterm_forecast/initial_pointrange_correct.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
  current_results %>%
    mutate(correct = if_else(act <= ci_upper & act >= ci_lower, "y", "n")) %>%
    filter(correct == "y") %>% 
    ggplot(aes(x = act,
               y = est,
               ymin = ci_lower,
               ymax = ci_upper,
               color = abs(est - act))) +
    geom_pointrange(size = 0.75,
                    alpha = 0.25) +
    geom_abline(linetype = "dashed",
                color = "gray") +
    scale_color_viridis_c() +
    labs(color = "error") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) + 
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    coord_equal() +
    facet_wrap(~race)
  
  ggsave("plots/midterm_forecast/initial_pointrange_correct_facet.png",
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

#################### DOWNWEIGHT FUNCTIONS ####################

# function for passing downweights for determining appropriate CI
pass_downweight <- function(race, cycle, region, begin_date, end_date, downweight) {
  
  target_region(race, pass_region(region), cycle) %>%
    poll_average(begin_date,
                 end_date,
                 pull_pollster_weights(variable_weights),
                 pull_sample_weight(),
                 pull_population_weights(variable_weights),
                 pull_methodology_weights(variable_weights),
                 pull_similarity_weight(),
                 pull_infer_weights(variable_weights),
                 pull_date_weight(),
                 downweight)
  
}

# summarise the error from testing 5 downweights
summarise_downweight <- function(.data, input_list) {
  
  # create summary tibble of errors
  weight_summary <- 
    
    # bind cols from input list for joining historical results
    .data %>%
    bind_cols(
      race = input_list$race,
      cycle = input_list$cycle,
      region = input_list$region,
      weight = input_list$weight
    ) %>%
    select(race:weight, starts_with("ci")) %>%
    
    # join historical results, define whether result was in or out of confidence interval
    left_join(historical_results, by = c("race", "cycle", "region")) %>%
    mutate(in_range = if_else(dem2pv <= ci_upper & dem2pv >= ci_lower, "in_range", "out")) %>%
    
    # summarise error based on 95% of actual values falling in ci
    group_by(weight) %>%
    percent(in_range) %>%
    ungroup() %>%
    pivot_wider(names_from = in_range,
                values_from = pct) %>%
    mutate(error = abs(0.95 - in_range)) %>%
    rowid_to_column()
  
  # determine the best weight
  best_weight <- 
    weight_summary %>%
    filter(error == min(error)) %>%
    filter(weight == min(weight)) %>%
    pull(weight)
  
  # determine best index
  best_index <-
    weight_summary %>%
    filter(weight == best_weight) %>%
    pull(rowid)
  
  # get the step between each weight
  delta <-
    weight_summary %>%
    mutate(delta = weight - lag(weight)) %>%
    drop_na() %>%
    filter(rowid == 5) %>%
    pull(delta)
  
  # assign next_upper & next_lower
  if (best_index == 5) {
    
    next_lower <- weight_summary %>% filter(rowid == 3) %>% pull(weight)
    next_upper <- weight_summary %>% filter(rowid == 5) %>% pull(weight) + (2 * delta)
    
  } else if (best_index == 1) {
    
    if (best_weight == 0) {
      
      next_lower <- 0
      next_upper <- weight_summary %>% filter(rowid == 2) %>% pull(weight)
      
    } else {
      
      next_lower <- weight_summary %>% filter(rowid == 1) %>% pull(weight) - (2 * delta)
      next_upper <- weight_summary %>% filter(rowid == 3) %>% pull(weight)
      
    }
    
  } else {
    
    next_lower <- weight_summary %>% filter(rowid == best_index - 1) %>% pull(weight)
    next_upper <- weight_summary %>% filter(rowid == best_index + 1) %>% pull(weight)
    
  }
  
  # determine pct diff between best/worst error
  best_error <-
    weight_summary %>%
    filter(weight == best_weight) %>%
    pull(error)
  
  worst_error <-
    weight_summary %>%
    filter(error == max(error)) %>%
    filter(weight == max(weight)) %>%
    pull(error)
  
  pct_diff <- abs(worst_error - best_error)/mean(c(best_error, worst_error))
  
  # note whether or not to contine based off difference threshold of 1%
  if (pct_diff < 0.01) {
    
    search_suggestion <- "final"
    
  } else {
    
    search_suggestion <- "not final"
    
  }
  
  # summarise in 1-row tibble
  weight_summary <-
    tibble(metric = "downweight",
           weight = best_weight,
           error = best_error,
           next_lower = next_lower,
           next_upper = next_upper,
           pct_diff = pct_diff,
           search_suggestion = search_suggestion)
  
  return(weight_summary)
  
}

# function for updating downweight
update_downweight <- function() {
  
  # do not evaluate if final
  if (check_suggestion("downweight") == "final") {
    
    message("downweight marked as final and will not be updated.")
    
  } else {
    
    # create a try list to pass to passer function
    try_list <- create_try_list("downweight")
    
    # map inputs to passer function
    weight_map <-
      try_list %>%
      future_pmap_dfr(~pass_downweight(..1, ..2, ..3, ..4, ..5, ..6))
    
    # create summary table
    weight_summary <-
      weight_map %>%
      summarise_downweight(try_list)
    
    # update tracking tibbles
    downweight_tracker <<-
      downweight_tracker %>%
      bind_rows(weight_summary)
    
    variable_weights <<-
      variable_weights %>%
      filter(variable != "downweight") %>%
      bind_rows(weight_summary %>% select(-error, -pct_diff) %>% rename(variable = metric))
    
    # save csvs to file
    downweight_tracker %>% write_csv("data/models/midterm_model/downweight_tracker.csv")
    variable_weights %>% write_csv("data/models/midterm_model/variable_weights.csv")
    
  }
  
}

#################### UPDATE CONFIDENCE INTERVALS ####################

# set to FALSE to rerun
completed <- TRUE

if (completed == FALSE) {
  
  plan(multisession, workers = 8)
  downweight_tracker <- read_csv("data/models/midterm_model/downweight_tracker.csv")
  variable_weights <- read_csv("data/models/midterm_model/variable_weights.csv")
  
  # round 1
  update_downweight()
  
  # round 2
  update_downweight()
  
  # round 3
  update_downweight()
  
  # round 4
  update_downweight()
  
  # round 5
  update_downweight()
  
  # round 6
  update_downweight()
  
  # round 7
  update_downweight()
  
  # round 8
  update_downweight()
  
  # round 9
  update_downweight()
  
}

#################### TESTING ZONG MY GUY ####################

test_try <- create_try_list("date_weight")

plan(multisession, workers = 8)
test_map <- 
  test_try %>%
  future_pmap_dfr(~pass_date_weight(..1, ..2, ..3, ..4, ..5, ..6))

test_map %>%
  bind_results(test_try) %>%
  summarise_weights("date_weight")

#################### notes ####################

# to-do:
#   remove dependencies on begin_date (taken care of by target_region)
#
#   change model to weight rmse by n polls
#     races with more polls will influence the model more
#     summarise weights
#     others?\
#
#   update poll average to include n_polls for downweighting
#     poll_average
#     pass functions
#     others ?
#
#   train mean
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
#   doesn't include common features that definitely have signal (e.g., gender, wages, prez approval)
#     due to lack of historical polling data (only 2 cycle's worth)
#   only 2 cycles worth of data
#     if there was over/under performance during these cycles, may throw model off
#     e.g., 2018 was a "blue wave" & represents a good chunk of the training data

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


