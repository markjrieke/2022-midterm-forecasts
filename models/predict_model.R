# -----------------------------setup--------------------------------------------

# libraries
library(tidyverse)
library(lubridate)
library(riekelib)
library(workboots)

# ~ the model ~
elections_model <-      read_rds("models/midterm_model.rds")

# residual models
residual_trn <-     read_rds("models/residual_trn.rds")
residual_oob <-     read_rds("models/residual_oob.rds")
mod_analysis <-     read_csv("models/data/mod_analysis.csv")
mod_assessment <-   read_csv("models/data/mod_assessment.csv")

# data (training)
variable_weights <-     read_csv("data/models/midterm_model/variable_weights.csv")
methods <-              read_rds("data/models/midterm_model/methods.rds")

# data (2022)
demographics <-             read_csv("models/data/demographics_2022.csv")
region_similarities <-      read_csv("models/data/similarities_2022.csv")
training <-                 read_rds("models/data/training.rds")
elections <-                read_csv("models/data/elections_2022.csv")
elections_daily_average <-  read_csv("models/data/elections_daily_average.csv")

# functions
target_region <-            read_rds("models/utils/target_region.rds")
pull_similarity_weight <-   read_rds("models/utils/pull_similarity_weight.rds")
pull_sample_weight <-       read_rds("models/utils/pull_sample_weight.rds")
pull_population_weights <-  read_rds("models/utils/pull_population_weights.rds")
pull_pollster_weights <-    read_rds("models/utils/pull_pollster_weights.rds")
pull_national_weight <-     read_rds("models/utils/pull_national_weight.rds")
pull_methodology_weights <- read_rds("models/utils/pull_methodology_weights.rds")
pull_infer_weights <-       read_rds("models/utils/pull_infer_weights.rds")
pull_downweight <-          read_rds("models/utils/pull_downweight.rds")
pull_date_weight <-         read_rds("models/utils/pull_date_weight.rds")
poll_average <-             read_rds("models/utils/poll_average.rds")
pass_region <-              read_rds("models/utils/pass_region.rds")
pass_current_fit <-         read_rds("models/utils/pass_current_fit.rds")

# fte candidate file
cand_senate <-  read_csv("https://projects.fivethirtyeight.com/2022-general-election-forecast-data/senate_steps_2022.csv")
cand_house <-   read_csv("https://projects.fivethirtyeight.com/2022-general-election-forecast-data/house_steps_2022.csv")
cand_gov <-     read_csv("https://projects.fivethirtyeight.com/2022-general-election-forecast-data/governor_steps_2022.csv")

# current polls
polls_senate <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
polls_house <-  read_csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv")
polls_gov <-    read_csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")
polls_gcb <-    read_csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls.csv")

# ------------------------------prep-polls--------------------------------------

# get to a list of candidates across all races
distinct_candidates <- function(.data) {
  
  .data %>%
    select(starts_with("candidate")) %>%
    pivot_longer(starts_with("candidate"),
                 names_to = "desc",
                 values_to = "name") %>%
    distinct(name) %>%
    pull(name)
  
}

# list of candidates per fte's "step" files
candidates <-
  c(cand_senate %>% distinct_candidates(),
    cand_house %>% distinct_candidates(),
    cand_gov %>% distinct_candidates())

# list of independents being treated as partisan dem/rep
independent_dems <-
  c("Evan McMullin",
    "Alexander M. Remrey", 
    "Andria Chieffo", 
    "Joseph \"Joe\" Hannoush", 
    "Keith R. Hayden Jr.", 
    "Lorence R. Wenke",
    "Michael Chandler",
    "Collin Duprel",
    "Takona Scauflaire",
    "Scott Collier")

independent_reps <-
  c("Michael Ernest Kerr",
    "Nader Akhlaghy",
    "Christopher Hoeppner") 

# merge senate/house/governor polls
polls <- 
  bind_rows(polls_senate %>% mutate(race = "Senate"),
            polls_house %>% mutate(race = "House"),
            polls_gov %>% mutate(race = "Governor")) %>% 
  
  # initial col selection & renaming
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
         candidate_party = party,
         pct) %>%
  
  # impute median sample size for NA
  mutate(sample_size = if_else(is.na(sample_size), median(.$sample_size, na.rm = TRUE), sample_size)) %>%
  
  # replace nas
  mutate(methodology = replace_na(methodology, "Unknown Method"),
         population = replace_na(population, "Unknown Population")) %>%
  
  # remedy seats
  mutate(seat = if_else(race == "Governor", "Governor", seat),
         seat = paste(state, seat)) %>%
  
  # col type fix
  mutate(pct = pct/100,
         end_date = lubridate::mdy(end_date)) %>%
  
  # filter to only polls starting in the summer 
  filter(end_date >= lubridate::ymd("2022-06-01")) %>%
  
  # fix specific candidate parties
  mutate(candidate_party = case_when(candidate_name %in% independent_dems ~ "DEM", 
                                     candidate_name %in% independent_reps ~ "REP",
                                     TRUE ~ candidate_party)) %>%
  
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
  
  # fix specific seat errors
  mutate(candidate_name = case_when(seat == "Rhode Island District 2" & !candidate_name %in% c("Allan W. Fung", "Seth M. Magaziner") ~ "remove",
                                    seat == "Michigan Governor" & !candidate_name %in% c("Tudor M. Dixon", "Gretchen Whitmer") ~ "remove",
                                    seat == "Missouri Class III" & !candidate_name %in% c("Eric Schmitt", "Trudy Busch Valentine") ~ "remove", 
                                    seat == "New York Governor" & !candidate_name %in% c("Lee M. Zeldin", "Kathy C. Hochul") ~ "remove", 
                                    seat == "Wisconsin Class III" & !candidate_name %in% c("Ron Johnson", "Mandela Barnes") ~ "remove", 
                                    seat == "Wisconsin Governor" & !candidate_name %in% c("Tim Michels", "Tony Evers") ~ "remove",
                                    seat == "Arizona Class III" & !candidate_name %in% c("Mark Kelly", "Blake Masters") ~ "remove",
                                    seat == "New Hampshire Class III" & !candidate_name %in% c("Maggie Hassan", "Donald C. Bolduc") ~ "remove",
                                    seat == "Utah Class III" & !candidate_name %in% c("Mike Lee", "Evan McMullin") ~ "remove",
                                    seat == "Colorado Class III" & !candidate_name %in% c("Michael Bennet", "Joe O'Dea") ~ "remove",
                                    seat == "Connecticut Class III" & !candidate_name %in% c("Richard Blumenthal", "Leora R. Levy") ~ "remove",
                                    seat == "Massachusetts Governor" & !candidate_name %in% c("Maura Healey", "Geoff Diehl") ~ "remove",
                                    seat == "Arizona Governor" & !candidate_name %in% c("Katie Hobbs", "Kari Lake") ~ "remove",
                                    seat == "Colorado Governor" & !candidate_name %in% c("Jared Polis", "Heidi Ganahl") ~ "remove",
                                    seat == "Florida Governor" & !candidate_name %in% c("Ron DeSantis", "Charlie Crist") ~ "remove",
                                    seat == "Alaska District 1" & !candidate_name %in% c("Nick Begich", "Mary S. Peltola") ~ "remove",
                                    seat == "Florida District 13" & !candidate_name %in% c("Anna Paulina Luna", "Eric Lynn") ~ "remove",
                                    seat == "Florida District 27" & !candidate_name %in% c("Marìa Elvira Salazar", "Annette Taddeo") ~ "remove",
                                    seat == "New York District 17" & !candidate_name %in% c("Michael V. Lawler", "Sean Patrick Maloney") ~ "remove",
                                    seat == "Texas District 34" & !candidate_name %in% c("Mayra Flores", "Vicente Gonzalez") ~ "remove",
                                    TRUE ~ candidate_name)) %>%
  filter(candidate_name != "remove") %>%
  
  # reformat to wide
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
         pct_REP) %>%
  filter(!if_any(starts_with("candidate"), is.na))

# wrangle national polls
polls_gcb <- 
  polls_gcb %>%
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
         end_date = lubridate::mdy(end_date),
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

# merge all polls together
polls <-
  bind_rows(polls, polls_gcb)

# clean up environment
rm(polls_gcb,
   polls_gov,
   polls_house,
   polls_senate)

# replace pollsters/methodologies that aren't used regularly with "other"
polls <- 
  polls %>%
  mutate(pollster = if_else(pollster %in% variable_weights$variable, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% variable_weights$variable, methodology, "Other Method"),
         dem2pv = pct_DEM/(pct_DEM + pct_REP)) %>%
  select(-starts_with("pct"))

# ------------------------------prep-2022-tibble--------------------------------

# save uncontested elections for later
uncontested <- 
  elections %>%
  filter(if_any(starts_with("candidate"), ~ . == "-"))

# format elections just to ones of interest
elections <- 
  elections %>%
  anti_join(uncontested, by = c("race", "state", "seat")) %>%
  select(cycle, race, state, seat, ends_with("incumbent")) %>%
  mutate(seat = paste(state, seat)) %>%
  select(-state)

# create a sequence of dates from 7/1 to today to get polling avg
days <- seq(ymd("2022-07-01"), as_date(Sys.Date()), by = "days")

# remove dates that already have polling averages
days <- days[!days %in% elections_daily_average$date]

# add new polling average to elections_daily_average
for (dates in days) {
  
  message(glue::glue("Creating poll average for {as_date(dates)}"))

  # create daily polling average
  wassp_average <-
    elections %>%
    select(race,
           cycle,
           region = seat) %>%
    mutate(begin_date = ymd("2022-06-01"),
           end_date = as_date(dates)) %>%
    as.list() %>%
    pmap_dfr(~pass_current_fit(polls, ..1, ..2, ..3, ..4, ..5))
  
  # append with election/demographic data
  elections_out <- 
    wassp_average %>%
    bind_cols(elections) %>%
    rename(region = seat,
           estimate = dem2pv) %>%
    relocate(race, cycle, region, .after = date) %>%
    mutate(join_region = region,
           join_region = str_remove_all(join_region, " Governor"),
           join_region = str_remove_all(join_region, " Class III"),
           join_region = str_remove_all(join_region, " Class II"),
           join_region = str_remove_all(join_region, " Class I")) %>%
    left_join(demographics, by = c("join_region" = "region")) %>%
    select(-join_region)
  
  # bind to daily average
  elections_daily_average <-
    elections_daily_average %>%
    bind_rows(elections_out)
  
}

# write daily averages out
elections_daily_average %>%
  write_csv("models/data/elections_daily_average.csv")

# -----------------------------predictions--------------------------------------

# adjust training results to unbound scale
training <- 
  training %>%
  mutate(result = logit(result))

# predict
set.seed(2022)
elections_preds <- 
  elections_model %>%
  predict_boots(
    n = 100,
    training_data = training,
    new_data = elections_daily_average,
    verbose = TRUE
  )

# -----------------------------testing-grounds----------------------------------

tmp <- 
  elections_preds %>%
  summarise_predictions() %>%
  select(.pred) %>%
  bind_cols(elections_daily_average)


tmp_n <- 
  tmp %>%
  select(n)

conf <- 0.95
lookup <- "Georgia Governor"

bind_cols(mu_trn = predict(residual_trn, newdata = tmp_n, what = "mu", type = "response"),
          sd_trn = predict(residual_trn, newdata = tmp_n, what = "sigma", type = "response"),
          mu_oob = predict(residual_oob, newdata = tmp_n, what = "mu", type = "response"),
          sd_oob = predict(residual_oob, newdata = tmp_n, what = "sigma", type = "response")) %>%
  mutate(resid_mu = 0.638 * mu_oob + (1-0.638) * mu_trn,
         resid_sd = 0.638 * sd_oob + (1-0.638) * sd_trn) %>%
  select(starts_with("resid")) %>%
  riekelib::normal_interval(resid_mu, resid_sd, conf = conf) %>%
  select(starts_with("ci")) %>%
  rename_with(~str_replace(.x, "ci", "resid")) %>%
  bind_cols(tmp) %>%
  mutate(.pred_lower = .pred + resid_lower,
         .pred_upper = .pred + resid_upper,
         across(starts_with(".pred"), riekelib::expit)) %>%
  filter(region == lookup) %>%
  ggplot(aes(x = date,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.15) +
  geom_point(data = polls %>% filter(seat == lookup, end_date > ymd("2022-07-01")),
             mapping = aes(x = end_date, y = dem2pv, ymin = NULL, ymax = NULL)) +
  expand_limits(y = c(0, 1))



tmp %>% 
  bind_cols(elections_daily_average) %>% 
  filter(region == lookup) %>% 
  mutate(across(starts_with(".pred"), expit)) %>% 
  ggplot() + 
  geom_line(aes(x = date, 
                y = .pred),
            size = 1) + 
  geom_ribbon(aes(x = date,
                  ymin = .pred_lower,
                  ymax = .pred_upper),
              alpha = 0.25) 

training_beta <- training %>% mutate(result = expit(result))
beta_weights <- training$n + 2

training_beta <- 
  training_beta %>%
  mutate(incumbent = case_when(dem_incumbent == "y" & rep_incumbent == "n" ~ "dem",
                               dem_incumbent == "n" & rep_incumbent == "y" ~ "rep",
                               dem_incumbent == "y" & rep_incumbent == "y" ~ "both",
                               dem_incumbent == "n" & rep_incumbent == "n" ~ "none"))

training_rec <- 
  recipe(result ~ ., data = training) %>% 
  step_mutate(across(c(white:other), logit),
              incumbent = case_when(dem_incumbent == "y" & rep_incumbent == "n" ~ "dem",
                                    dem_incumbent == "n" & rep_incumbent == "y" ~ "rep",
                                    dem_incumbent == "y" & rep_incumbent == "y" ~ "both",
                                    dem_incumbent == "n" & rep_incumbent == "n" ~ "none"),
              poll_bucket = case_when(n < 1 ~ "none",
                                      n < 10 ~ "some",
                                      TRUE ~ "many")) 

prepper <- function(.data) {
  
  .data %>%
    mutate(across(c(white:other), logit),
           incumbent = case_when(dem_incumbent == "y" & rep_incumbent == "n" ~ "dem",
                                 dem_incumbent == "n" & rep_incumbent == "y" ~ "rep",
                                 TRUE ~ "mix or neither"),
           poll_bucket = case_when(n < 1 ~ "none",
                                   n < 10 ~ "some",
                                   TRUE ~ "many"))
  
}
  
training_beta <-
  training %>%
  prepper() %>%
  mutate(result = expit(result)) %>%
  select(result, estimate, white, black, aapi, hispanic, poll_bucket, incumbent, n)

new_dawg <-
  elections_daily_average %>%
  prepper() %>%
  select(estimate, white, black, aapi, hispanic, poll_bucket, incumbent, n)

beta_model <- 
  gamlss::gamlss(result ~ estimate + white + black + aapi + hispanic + poll_bucket:incumbent,
                 sigma.formula = ~ log10(n + 2),
                 family = gamlss.dist::BE(),
                 weights = log10(beta_weights),
                 data = training_beta) 

summary(beta_model)

conf <- 0.95

lookup = "Ohio Governor"

tibble(.pred = beta_model %>% predict(newdata = as.data.frame(new_dawg), what = "mu", type = "response"),
       sigma = beta_model %>% predict(newdata = new_dawg, what = "sigma", type = "response")) %>%
  mutate(.pred_lower = gamlss.dist::qBE((1-conf)/2, .pred, sigma),
         .pred_upper = gamlss.dist::qBE((1-conf)/2 + conf, .pred, sigma)) %>%
  bind_cols(elections_daily_average) %>%
  filter(region == lookup) %>%
  ggplot(aes(x = date, 
             y = .pred,
             ymin = .pred_lower, 
             ymax = .pred_upper)) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.25) +
  expand_limits(y = c(0, 1)) +
  geom_point(data = polls %>% filter(seat == lookup, end_date > ymd("2022-07-01")),
             mapping = aes(x = end_date, y = dem2pv, ymin = NULL, ymax = NULL),
             size = 3.5,
             alpha = 0.25) +
  expand_limits(y = c(0, 1))
  
  
  ggplot(aes(x = result,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_point(aes(size = n),
             alpha = 0.25) +
  geom_errorbar(alpha = 0.15) +
  geom_abline() +
  scale_size_continuous(range = c(1, 10)) 

training_beta %>%
  pivot_longer(c(white:other),
               names_to = "race_group",
               values_to = "race_pct") %>%
  ggplot(aes(x = race_pct,
             y = result,
             color = race_group,
             size = n)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~race_group, scales = "free_x") +
  geom_smooth(method = "gam",
              formula = y ~ ns(x, df = 0))
  
