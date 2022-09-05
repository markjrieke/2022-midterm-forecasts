# -----------------------------notes/to-do--------------------------------------

# investigate places with few polls (potential for primary polls to be slipping in)
# example: OH 9

# -----------------------------setup--------------------------------------------

library(gamlss)
library(tidyverse)

# set run date
run_date <- lubridate::mdy("9/4/22")
# run_date <- Sys.Date()

# polling data 
polls_house     <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv")
polls_senate    <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
polls_governor  <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")
polls_gcb       <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls.csv")

# model data
elections       <- read_csv("models/data/elections_2022.csv", locale = locale(encoding = "latin1"))
demographics    <- read_csv("models/data/demographics_2022.csv")
pvi             <- read_csv("models/data/pvi_2022.csv")
elections_train <- read_csv("models/data/elections_train.csv")

# helper models
poll_model    <- read_rds("models/data/poll_model.rds")
gcb_model     <- read_rds("models/data/gcb_model.rds")
polling_error <- read_rds("models/data/polling_error.rds")

# ------------------------wrangle-polled-races----------------------------------

# get each candidate's polling average in each race
set.seed(999)
poll_leaders <- 
  
  # create master frame of all polls & select relevant columns
  bind_rows(polls_house %>% mutate(race = "House"),
            polls_governor %>% mutate(race = "Governor"),
            polls_senate %>% mutate(race = "Senate")) %>%
  select(question_id,
         cycle,
         race,
         state,
         seat_name,
         end_date,
         election_date,
         candidate_name,
         candidate_party = party,
         pct) %>%
  
  # fix missing dates
  mutate(election_date = if_else(is.na(election_date), "11/8/22", election_date)) %>%
  
  # remove special elections
  filter(election_date == "11/8/22") %>%
  
  # reformat col types
  mutate(across(ends_with("date"), lubridate::mdy),
         pct = pct/100,
         seat_name = replace_na(seat_name, "Governor")) %>%
    
  # filter to only races polled during the race year/polls before run date
  filter(cycle == lubridate::year(end_date),
         end_date <= run_date) %>%
    
  # join candidate names for filtering
  left_join(elections, by = c("cycle", "race", "state", "seat_name" = "seat")) %>%
  select(question_id:candidate_name_REP) %>%
    
  # only keep candidates that are getting modeled
  mutate(across(starts_with("candidate_name_"), ~sub(".* ", "", .x)),
         match_name = case_when(str_detect(candidate_name, candidate_name_DEM) ~ 1,
                                str_detect(candidate_name, candidate_name_REP) ~ 1,
                                TRUE ~ 0)) %>%
  filter(match_name == 1) %>%
    
  # only keep polls that include both candidates
  group_by(question_id) %>%
  mutate(match_name = sum(match_name)) %>%
  filter(match_name == 2) %>%
  select(-match_name, -starts_with("candidate_name_")) %>%
    
  # change to 2pv
  mutate(pct = pct/sum(pct)) %>%
  ungroup() %>%
  select(-question_id) %>%
  
  # get metadata abt polls to be used in model later
  nest(data = c(end_date, pct)) %>%
  mutate(num_polls = map_dbl(data, nrow),
         last_poll = map_dbl(data, ~max(.x$end_date)),
         last_poll = lubridate::as_date(last_poll)) %>%
  
  # model!
  mutate(model = map(data, poll_model)) %>%
  
  # predict current poll average
  nplyr::nest_filter(data, end_date == max(end_date)) %>%
  nplyr::nest_distinct(data, end_date, .keep_all = TRUE) %>%
  mutate(poll_avg = pmap_dbl(list(model, data), ~predict(..1, ..2))) %>%
  select(-data, -model)

# clean up environment
rm(polls_governor, polls_house, polls_senate)

# ------------------------------wrangle-gcb-------------------------------------

set.seed(888)
gcb <-
  polls_gcb %>%
  gcb_model("1/1/21", paste(lubridate::month(run_date), lubridate::day(run_date), 22, sep = "/"))

# ------------------------------prep-train-data---------------------------------

# remove any races that won't be modeled
elections_mod <- 
  elections %>%
  filter(if_all(starts_with("candidate_name"), ~ .x != "-"))

# wrangle poll_leaders for joining to elections_mod
poll_averages <- 
  poll_leaders %>%
  
  # reformat cols
  rename(seat = seat_name) %>%
  mutate(candidate_party = str_to_lower(candidate_party),
         candidate_party = if_else(candidate_party %in% c("dem", "rep"), candidate_party, "ind"))
  
# write out for manual candidate checking
poll_averages %>%
  left_join(elections, by = c("cycle", "race", "state", "seat")) %>%
  write_csv("models/poll_health_check.csv")

# continue wrangling
poll_averages <- 
  poll_averages %>%
  
  # summarise at the race level
  select(-candidate_name) %>%
  pivot_wider(names_from = candidate_party,
              values_from = poll_avg) %>% 
  mutate(across(c(dem, rep, ind), ~replace_na(.x, 0))) %>% 
  
  # assign independents
  mutate(dem = if_else(dem == 0 & ind != 0, ind, dem),
         rep = if_else(rep == 0 & ind != 0, ind, rep)) %>% 
  
  # get the polling delta
  mutate(last_poll = election_date - last_poll) %>%
  select(cycle,
         race,
         state,
         seat,
         num_polls,
         last_poll,
         dem,
         rep)

# prep pvi for merging with training frame
pvi <-
  pvi %>%
  mutate(join_region = replace_na(seat, "State")) %>%
  select(-seat)

# states with one cd
one_cd <- 
  c("Alaska",
    "Delaware",
    "North Dakota",
    "South Dakota",
    "Vermont",
    "Wyoming")

# prep training frame
elections_predict <- 
  elections_mod %>%
  select(-pending) %>%
  
  # append with polling average and mend NAs
  left_join(poll_averages, by = c("cycle", "race", "state", "seat")) %>%
  mutate(num_polls = replace_na(num_polls, 0),
         last_poll = as.numeric(last_poll),
         last_poll = replace_na(last_poll, max(elections_train$last_poll))) %>%
  
  # append with pvi
  mutate(join_region = if_else(state %in% one_cd | !str_detect(seat, "District"), "State", seat)) %>%
  left_join(pvi, by = c("cycle", "state", "join_region")) %>%
  select(-join_region) %>%
  
  # append with gcb and fill in NAs for poll avg
  left_join(gcb, by = "cycle", suffix = c("", "_gcb")) %>%
  mutate(dem_gcb = dem_gcb + pvi/2,
         rep_gcb = rep_gcb - pvi/2,
         across(ends_with("gcb"), ~ .x/100),
         dem = if_else(is.na(dem), dem_gcb, dem),
         rep = if_else(is.na(rep), rep_gcb, rep)) %>%
  select(-ends_with("gcb"), -pvi) %>%
  
  # append with demographic data
  mutate(join_region = if_else(state %in% one_cd | !str_detect(seat, "District"),
                               state,
                               paste(state, seat))) %>%
  left_join(demographics, by = c("join_region" = "region")) %>%
  select(-join_region) %>%
  
  # get dem2pv estimate
  mutate(estimate = dem/(dem + rep)) %>%
  relocate(estimate, .after = candidate_name_REP) %>%
  select(-dem, -rep) %>%
  
  # get incumbency
  mutate(incumbent = case_when(dem_incumbent == "y" & rep_incumbent == "n" ~ "dem",
                               dem_incumbent == "n" & rep_incumbent == "y" ~ "rep",
                               TRUE ~ "mix or neither")) %>%
  relocate(incumbent, .after = estimate) %>%
  select(-ends_with("_incumbent")) %>%
  
  # get poll bucket/fix days
  mutate(poll_bucket = case_when(num_polls < 1 ~ "none",
                                 TRUE ~ "polled")) %>%
  
  # adjust racial groups to be on unbounded scales
  mutate(across(c(white:other), riekelib::logit)) %>% 
  
  #remove unneeded cols
  select(-c(cycle:candidate_name_REP))
  
# -----------------------------------model!-------------------------------------

set.seed(666) # punk
elections_model <- 
  gamlss(result ~ estimate*poll_bucket + incumbent + white + black + hispanic + aapi,
         sigma.formula = ~ log10(num_polls + 2),
         family = BE(),
         data = elections_train)

# --------------------------------simulations-----------------------------------

# number of races being modeled (may change if updated by fte)
n_races <- nrow(elections_predict)

# generate random polling errors
set.seed(555)
sim_data <-
  tibble(sim = rep(seq(1, 10000), n_races),
         idx = seq(1, n_races) %>% rep(10000) %>% riekelib::arrange_vector(),
         err = rep(rnorm(10000, 0, 0.5 * polling_error$err_sd), n_races)) %>%
  nest(data = c(sim, err))

# apply random polling error to poll avg
sim_data <- 
  sim_data %>%
  bind_cols(elections_predict) %>%
  unnest(data) %>%
  mutate(estimate = estimate + err,
         estimate = case_when(estimate < 0 ~ 0,
                              estimate > 1 ~ 1,
                              TRUE ~ estimate)) 

# remove cols for predicting
sim_mod <-
  sim_data %>%
  select(-idx, -sim, -err)

# make predictions on simulated data
sim_preds <-
  tibble(mu = predict(elections_model, newdata = sim_mod, what = "mu", type = "response"),
         sigma = predict(elections_model, newdata = sim_mod, what = "sigma", type = "response"))

# generate random draws 
set.seed(444)
sim_preds <- 
  sim_preds %>%
  bind_cols(sim_data) %>%
  select(mu:sim) %>%
  bind_cols(p = runif(n_races * 10000)) %>%
  mutate(.pred = qBE(p, mu, sigma))

# bind to elections data
sim_preds <- 
  sim_preds %>%
  select(-p) %>%
  left_join(rowid_to_column(elections_mod),
            by = c("idx" = "rowid")) %>%
  mutate(winner = if_else(.pred >= 0.5, "dem", "rep"),
         model_date = run_date) %>%
  select(cycle, 
         race,
         state,
         seat,
         model_date,
         starts_with("candidate_name"),
         sim,
         mu,
         sigma,
         .pred,
         winner) %>%
  nest(data = -c(cycle:model_date)) 

# ------------------------------write-out-results-------------------------------

# get current run's candidate distribution
new_candidate_predictions <- 
  sim_preds %>%
  mutate(p_dem_win = map_dbl(data, ~sum(.x$.pred >= 0.5)),
         p_dem_win = p_dem_win/10000,
         .pred = map_dbl(data, ~quantile(.x$.pred, probs = 0.5)),
         .pred_lower = map_dbl(data, ~quantile(.x$.pred, probs = 0.1)),
         .pred_upper = map_dbl(data, ~quantile(.x$.pred, probs = 0.9))) %>%
  select(-data) %>%
  left_join(elections_mod, by = c("cycle", "race", "state", "seat")) %>%
  select(-ends_with("incumbent"), -pending) %>%
  relocate(starts_with("candidate_name"), .after = model_date) 

# append candidate file
read_csv("models/outputs/candidate_predictions.csv") %>%
  filter(model_date != run_date) %>%
  bind_rows(new_candidate_predictions) %>%
  write_csv("models/outputs/candidate_predictions.csv")

# get the number of races where dems are uncontested
dem_uncontested <-
  elections %>%
  filter(race == "House",
         candidate_name_REP == "-") %>%
  nrow()

# get current run's house distribution
house_distribution <- 
  sim_preds %>%
  filter(race == "House") %>%
  unnest(data) %>%
  count(model_date, race, sim, winner) %>%
  filter(winner == "dem") %>%
  mutate(n = n + dem_uncontested) 

# overwrite current distribution
house_distribution %>%
  write_csv("models/outputs/current_house_distribution.csv")

# current run's house topline
new_house_topline <- 
  house_distribution %>%
  select(-sim, -winner) %>%
  nest(data = n) %>%
  mutate(p_dem_win = map_dbl(data, ~sum(.x$n > 435/2)),
         p_dem_win = p_dem_win/10000,
         seats = map_dbl(data, ~quantile(.x$n, probs = 0.5)),
         seats_lower = map_dbl(data, ~quantile(.x$n, probs = 0.1)),
         seats_upper = map_dbl(data, ~quantile(.x$n, probs = 0.9))) %>%
  select(-data)

# append house topline file
read_csv("models/outputs/house_topline.csv") %>%
  filter(model_date != run_date) %>%
  bind_rows(new_house_topline) %>% 
  write_csv("models/outputs/house_topline.csv")

# get number of senate races where dems are uncontested
dem_uncontested <- 
  elections %>%
  filter(race == "Senate",
         candidate_name_REP == "-") %>%
  nrow()

# get current run's senate distribution
senate_distribution <- 
  sim_preds %>%
  filter(race == "Senate") %>%
  unnest(data) %>%
  count(model_date, race, sim, winner) %>%
  filter(winner == "dem") %>%
  mutate(n = n + dem_uncontested + 36)

# some sims will contain 0 dem wins - add 0 win rows here
no_wins <- 10000 - nrow(senate_distribution)

# get sims up to 10000
senate_distribution <- 
  tibble(model_date = rep(run_date, no_wins),
         race = rep("Senate", no_wins),
         sim = rep(0, no_wins),
         n = rep(36, no_wins),
         winner = rep("rep", no_wins)) %>%
  bind_rows(senate_distribution)

# overwrite current distribution
senate_distribution %>%
  write_csv("models/outputs/current_senate_distribution.csv")

# get current run's senate topline
new_senate_topline <- 
  senate_distribution %>%
  select(-sim, -winner) %>%
  nest(data = n) %>%
  mutate(p_dem_win = map_dbl(data, ~sum(.x$n >= 50)),
         p_dem_win = p_dem_win/10000,
         seats = map_dbl(data, ~mean(.x$n)),
         seats_sd = map_dbl(data, ~sd(.x$n)),
         seats_lower = qnorm(0.1, seats, seats_sd),
         seats_upper = qnorm(0.9, seats, seats_sd)) %>%
  select(-data, -seats_sd)

# append senate topline file
read_csv("models/outputs/senate_topline.csv") %>%
  filter(model_date != run_date) %>%
  bind_rows(new_senate_topline) %>%
  write_csv("models/outputs/senate_topline.csv")

# ---------------------------------diagnostics----------------------------------

# current house distribution
house_distribution %>%
  mutate(control = if_else(n > 218.5, "blue", "red")) %>%
  ggplot(aes(x = n,
             fill = control)) +
  geom_histogram(binwidth = 1,
                 alpha = 0.5) +
  scale_fill_identity() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white")) +
  labs(title = paste("House distribution as of", run_date))

riekelib::ggquicksave("models/diagnostics/current_house_topline.png")

# current senate distribution
senate_distribution %>%
  mutate(control = case_when(n > 50 ~ "blue",
                             n < 50 ~ "red",
                             TRUE ~ "purple")) %>%
  ggplot(aes(x = n,
             fill = control)) +
  geom_histogram(binwidth = 1,
                 alpha = 0.5,
                 color = "white") +
  scale_fill_identity() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white")) + 
  labs(title = paste("Senate distribution as of", run_date))

riekelib::ggquicksave("models/diagnostics/current_senate_topline.png")

# rolling house distribution
read_csv("models/outputs/house_topline.csv") %>%
  rename_with(.cols = starts_with("seats"),
              .fn = ~paste0(.x, "_dem")) %>%
  mutate(seats_rep = 435 - seats_dem,
         seats_lower_rep = 435 - seats_upper_dem,
         seats_upper_rep = 435 - seats_lower_dem) %>%
  ggplot(aes(x = model_date)) +
  geom_ribbon(aes(ymin = seats_lower_rep,
                  ymax = seats_upper_rep),
              fill = "red",
              alpha = 0.25) +
  geom_ribbon(aes(ymin = seats_lower_dem,
                  ymax = seats_upper_dem),
              fill = "blue",
              alpha = 0.25) +
  geom_line(aes(y = seats_rep),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = seats_rep),
            size = 1.25,
            color = "red") +
  geom_line(aes(y = seats_dem),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = seats_dem),
            size = 1.25,
            color = "blue") +
  geom_vline(xintercept = run_date,
             linetype = "dashed",
             color = "gray") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white")) +
  labs(title = paste("House seats through", run_date),
       x = NULL,
       y = NULL) +
  expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")))

riekelib::ggquicksave("models/diagnostics/rolling_house_distribution.png")

# rolling house probability
read_csv("models/outputs/house_topline.csv") %>%
  mutate(p_rep_win = 1 - p_dem_win) %>%
  ggplot(aes(x = model_date)) +
  geom_line(aes(y = p_rep_win),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = p_rep_win),
            size = 1.25,
            color = "red") +
  geom_line(aes(y = p_dem_win),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = p_dem_win),
            size = 1.25,
            color = "blue") +
  theme_minimal() +
  geom_vline(aes(xintercept = run_date),
             linetype = "dashed",
             color = "gray") +
  theme(plot.background = element_rect(fill = "white", color = "white")) +
  labs(title = paste("House win probability through", run_date),
       x = NULL,
       y = NULL) +
  expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")))

riekelib::ggquicksave("models/diagnostics/rolling_house_probability.png")  

# rolling senate distribution
read_csv("models/outputs/senate_topline.csv") %>%
  rename_with(.cols = starts_with("seats"),
              .fn = ~paste0(.x, "_dem")) %>%
  mutate(seats_rep = 100 - seats_dem,
         seats_lower_rep = 100 - seats_upper_dem,
         seats_upper_rep = 100 - seats_lower_dem) %>%
  ggplot(aes(x = model_date)) +
  geom_ribbon(aes(ymin = seats_lower_rep,
                  ymax = seats_upper_rep),
              fill = "red",
              alpha = 0.25) +
  geom_ribbon(aes(ymin = seats_lower_dem,
                  ymax = seats_upper_dem),
              fill = "blue",
              alpha = 0.25) +
  geom_line(aes(y = seats_rep),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = seats_rep),
            size = 1.25,
            color = "red") +
  geom_line(aes(y = seats_dem),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = seats_dem),
            size = 1.25,
            color = "blue") +
  theme_minimal() +
  geom_vline(aes(xintercept = run_date),
             linetype = "dashed",
             color = "gray") +
  theme(plot.background = element_rect(fill = "white", color = "white")) +
  labs(title = paste("Senate seats through", run_date),
       x = NULL,
       y = NULL) +
  expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")),
                y = c(40, 60))

riekelib::ggquicksave("models/diagnostics/rolling_senate_distribution.png")

# rolling senate probability 
read_csv("models/outputs/senate_topline.csv") %>%
  mutate(p_rep_win = 1 - p_dem_win) %>%
  ggplot(aes(x = model_date)) +
  geom_line(aes(y = p_rep_win),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = p_rep_win),
            size = 1.25,
            color = "red") +
  geom_line(aes(y = p_dem_win),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = p_dem_win),
            size = 1.25,
            color = "blue") +
  theme_minimal() +
  geom_vline(aes(xintercept = run_date),
             linetype = "dashed",
             color = "gray") +
  theme(plot.background = element_rect(fill = "white", color = "white")) +
  labs(title = paste("Senate win probability through", run_date),
       x = NULL,
       y = NULL) +
  expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")),
                y = c(0, 1))

riekelib::ggquicksave("models/diagnostics/rolling_senate_probability.png")

# random race
set.seed(run_date)
rand_race <- 
  read_csv("models/outputs/candidate_predictions.csv") %>%
  nest(data = -c(cycle, race, state, seat, starts_with("candidate"))) %>%
  slice_sample(n = 1) %>%
  unnest(data) %>%
  rename_with(.cols = starts_with(".pred"),
              .fn = ~paste0(.x, "_dem")) %>%
  mutate(.pred_rep = 1 - .pred_dem,
         .pred_lower_rep = 1 - .pred_upper_dem,
         .pred_upper_rep = 1 - .pred_lower_dem)

rand_race_title <- 
  paste(rand_race$state[1], rand_race$race[1], rand_race$seat[1]) 
  
rand_race %>%
  ggplot(aes(x = model_date)) +
  geom_ribbon(aes(ymin = .pred_lower_rep,
                  ymax = .pred_upper_rep),
              fill = "red",
              alpha = 0.25) +
  geom_ribbon(aes(ymin = .pred_lower_dem,
                  ymax = .pred_upper_dem),
              fill = "blue",
              alpha = 0.25) +
  geom_line(aes(y = .pred_rep),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = .pred_rep),
            size = 1.25,
            color = "red") +
  geom_line(aes(y = .pred_dem),
            size = 3.25,
            color = "white") +
  geom_line(aes(y = .pred_dem),
            size = 1.25,
            color = "blue") +
  geom_vline(aes(xintercept = run_date),
             linetype = "dashed",
             color = "gray") +
  labs(title = rand_race_title,
       subtitle = run_date) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white")) +
  expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")))

riekelib::ggquicksave("models/diagnostics/random_race.png")

# ---------------------------------junk-drawer----------------------------------

run_date %>% write_rds("models/outputs/run_date.rds")






