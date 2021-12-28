# libraries ----
library(tidyverse)
library(lubridate)
library(tidycensus)
library(riekelib)

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
         internal, 
         partisan, 
         candidate_name, 
         candidate_party, 
         pct) %>%
  rename(pollster = display_name,
         population = population_full,
         seat = seat_name) %>%
  mutate(partisan = replace_na(partisan, "NON"),
         methodology = replace_na(methodology, "Unknown"),
         pct = pct/100,
         end_date = mdy(end_date)) %>%
  filter(!is.na(sample_size),
         state != "Puerto Rico")

# get list of pollsters who have conducted at least 1% of polls
pollsters <- 
  house_polls %>%
  distinct(question_id, .keep_all = TRUE) %>%
  percent(pollster) %>%
  arrange(desc(pct)) %>%
  filter(pct > 0.01) %>%
  pull(pollster)

# get list of methodologies that makeup at least 1% of polls
methods <- 
  house_polls %>%
  distinct(question_id, .keep_all = TRUE) %>%
  percent(methodology) %>%
  arrange(desc(pct)) %>%
  filter(pct > 0.01) %>%
  pull(methodology)

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
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  pivot_wider(names_from = candidate_party,
              values_from = c(pct, candidate_name)) %>%
  mutate(across(starts_with("candidate"), ~replace_na(.x, "Unopposed")),
         across(starts_with("pct"), ~replace_na(.x, 0))) %>%
  select(-question_id) %>%
  relocate(starts_with("candidate"), .after = state) 

# coalesce partisan status, mutate to dem2pv
house_polls <- 
  house_polls %>%
  mutate(poll_type = case_when(partisan == "NON" ~ "non-partisan",
                               internal == FALSE & partisan == "DEM" ~ "external-DEM",
                               internal == FALSE & partisan == "REP" ~ "external-REP",
                               internal == TRUE & partisan == "DEM" ~ "internal-DEM",
                               internal == TRUE & partisan == "REP" ~ "internal-REP")) %>%
  mutate(dem2pv = pct_DEM/(pct_DEM + pct_REP)) %>%
  select(-internal, -partisan, - starts_with("pct"))

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
  arrange(region)
  


#################### TESTING ZONG MY GUY ####################


  left_join(demographics, by = c("variable" = "code")) %>%
  select(region = NAME,
         race, 
         value) %>%
  pivot_wider(names_from = race,
              values_from = value) %>%
  mutate(across(white:pac_islander, ~ .x/total),
         aapi = asian + pac_islander,
         other = 1 - white - black - hispanic - asian - pac_islander) %>%
  select(-total, -asian, -pac_islander)

get_decennial(geography = "congressional district",
              variables = demo_pl,
              year = 2020) %>%
  left_join(demographics, by = c("variable" = "code")) %>%
  select(region = NAME,
         race, 
         value) %>%
  pivot_wider(names_from = race,
              values_from = value) %>%
  mutate(across(white:pac_islander, ~ .x/total),
         aapi = asian + pac_islander,
         other = 1 - white - black - hispanic - asian - pac_islander) %>%
  select(-total, -asian, -pac_islander) %>%
  arrange(region)
  

# check vars
vars_2018 <- load_variables(2018, "acs1")
vars_2019 <- load_variables(2019, "acs1")
vars_2020 <- load_variables(2020, "pl")
