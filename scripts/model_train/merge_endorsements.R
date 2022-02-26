# libraries ----
library(tidyverse)
library(riekelib)

# read in data ----
historical_results <- 
  read_csv("data/models/midterm_model/historical_results.csv")

endorsements_scrape <-
  read_csv("data/endorsements/endorsements_scrape.csv")

endorsements_manual <-
  read_csv("data/endorsements/endorsements_manual.csv")

endorsements_recodes <-
  read_csv("data/endorsements/endorsements_recodes.csv")

# merge endorsements ----
endorsements <- 
  
  # merge rames together
  endorsements_scrape %>%
  bind_rows(endorsements_manual) %>%
  
  # remove line breaks
  mutate(endorsements = str_split(endorsements, "\\\n")) %>%
  unnest(endorsements) %>%
  
  # remove everything after commas, punctuation/digits
  mutate(endorsements = str_remove(endorsements, "\\,.*"),
         endorsements = str_remove_all(endorsements, "[:punct:]|[:digit:]")) %>%
  
  # remove endorsement categories
  filter(!endorsements %in% c("",
                              "Executive Branch officials",
                              "Executive officials",
                              "Federal Officials",
                              "Federal officials",
                              "Federal politicians",
                              "Federallevel officials",
                              "Former US Executive Branch officials",
                              "Governors",
                              "Individuals",
                              "Labor Unions",
                              "Labor unions",
                              "Local and statewide politicians",
                              "Local Officials",
                              "Local officials",
                              "Locallevel officials",
                              "Newspapers",
                              "Notable individuals",
                              "Organizations", 
                              "Other individuals",
                              "State legislators",
                              "State Officials",
                              "State officials",
                              "State Representatives",
                              "State Senators",
                              "Statelevel officials",
                              "Statewide and local politicians",
                              "Statewide officials",
                              "Unions",
                              "US Executive Branch Officials",
                              "US Executive Branch officials",
                              "US Military Personnel",
                              "US Presidents",
                              "US Representatives",
                              "US Representatives",
                              "US Senators",
                              "US Vice Presidents",
                              "US Governors")) %>%
  
  # fix 314 actions
  mutate(endorsements = if_else(endorsements == " Action", "314 Action", endorsements)) %>%
  nest(endorsements = endorsements) %>%
  
  # remove notes (e.g., [17]) and parties (e.g., (D)) from candidate names
  mutate(candidates = str_remove_all(candidates, "[:digit:]|\\[|\\]| \\s*\\([^\\)]+\\)| won primary")) %>%
  
  # recode candidate names
  left_join(endorsements_recodes, by = c("candidates" = "endorsement_name")) %>%
  mutate(candidates = if_else(is.na(results_name), candidates, results_name)) %>%
  select(-results_name)

historical_results %>% 
  left_join(endorsements, by = c("cycle", "race", "state", "candidate_name_DEM" = "candidates")) %>% 
  rename(endorsements_DEM = endorsements) %>% 
  left_join(endorsements, by = c("cycle", "race", "state", "candidate_name_REP" = "candidates")) %>% 
  rename(endorsements_REP = endorsements) %>% 
  mutate(across(starts_with("endorsements"), ~if_else(.x == "NULL", list(endorsements = c("none")), .x))) %>%
  
  unnest(endorsements_DEM) %>% 
  select(-endorsements_DEM) %>%
  nest(endorsements_DEM = endorsements) %>% 
  unnest(endorsements_REP) %>%
  select(-endorsements_REP) %>%
  nest(endorsements_REP = endorsements) %>%
  
  filter(cycle == 2018, race == "Governor", state == "Connecticut") %>%
  select(starts_with("candidate"), starts_with("endorsements"))
  
  count(cycle, race, state, seat) %>%
  count(n)
  
  filter(endorsements_DEM == "none") %>%
  select(cycle, race, state, seat, candidate_name_DEM) %>% filter(str_detect(candidate_name_DEM, "Spenser"))
  write_csv("temp.csv")

endorsements %>%
  filter(cycle == 2018,
         race == "Governor",
         state == "Oklahoma") #%>%
  filter(str_detect(candidates, "Brown"))
  
endorsements %>%
  filter(race == "Governor",
         state == "Illinois")

