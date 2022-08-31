# -----------------------------setup--------------------------------------------

library(gamlss)
library(tidyverse)

# polling data
polls_house     <- read_csv("data/polls/src/fte/house_polls_historical.csv")
polls_senate    <- read_csv("data/polls/src/fte/senate_polls_historical.csv")
polls_governor  <- read_csv("data/polls/src/fte/governor_polls_historical.csv")
polls_gcb       <- read_csv("data/polls/src/fte/generic_ballot_polls_historical.csv")

# training data
elections           <- read_csv("data/models/midterm_model/historical_results.csv")
demographics        <- read_csv("data/models/midterm_model/demographics.csv")
pvi                 <- read_csv("data/models/midterm_model/pvi.csv")

# ------------------------wrangle-polled-races----------------------------------

poll_model <- function(data) {
  
  if(nrow(data) < 10) {
    
    lm(pct ~ 1, data = data)
    
  } else {
    
    loess(pct ~ as.numeric(end_date), data = data)
    
  }
  
}

# get the top two candidates in each race by election day
set.seed(999)
poll_leaders <- 
  
  # create master frame of all polls & select relevant columns
  bind_rows(polls_house %>% mutate(race = "House"),
            polls_governor %>% mutate(race = "Governor"),
            polls_senate %>% mutate(race = "Senate")) %>% 
  select(cycle,
         race,
         state, 
         seat_name, 
         end_date, 
         election_date, 
         candidate_name,
         candidate_party,
         pct) %>%
    
  arrange(cycle, race, state, seat_name) %>%
  
  # fix missing dates in cycles
  mutate(election_date = case_when(is.na(election_date) & cycle == 2018 ~ "11/6/18",
                                   is.na(election_date) & cycle == 2020 ~ "11/3/20",
                                   TRUE ~ election_date)) %>%
  
  # remove special elections
  filter(election_date %in% c("11/6/18", "11/3/20")) %>%
  
  # fix specific races
  mutate(remove = case_when(cycle == 2018 & state == "New York" &
                              seat_name == "District 27" &
                              !candidate_name %in% c("Chris Collins",
                                                     "Nate McMurray") ~ "Remove",
                            cycle == 2018 & state == "Delaware" & 
                              seat_name == "District 1" &
                              !candidate_name %in% c("Lisa Blunt Rochester",
                                                     "Scott Walker") ~ "Remove", 
                            cycle == 2018 & state == "Louisiana" &
                              seat_name == "District 3" ~ "Remove", 
                            cycle == 2020 & state == "Alabama" & 
                              seat_name == "Class II" &
                              !candidate_name %in% c("Gordon Douglas Jones",
                                                     "Tommy Tuberville") ~ "Remove",
                            cycle == 2020 & state == "Michigan" & 
                              seat_name == "District 6" & 
                              !candidate_name %in% c("Fred Upton", 
                                                     "Jon Hoadley") ~ "Remove",
                            TRUE ~ "keep")) %>%
  filter(remove != "Remove") %>%
  select(-remove) %>%

  # reformat col types
  mutate(across(ends_with("date"), lubridate::mdy),
         pct = pct/100) %>%
    
  # filter out polls before july
  filter(!(cycle == 2018 & end_date < lubridate::mdy("6/1/18")),
         !(cycle == 2020 & end_date < lubridate::mdy("6/1/20"))) %>%
  
  # get some metadata abt polls to be used in model later
  nest(data = c(end_date, pct)) %>%
  mutate(num_polls = map_dbl(data, nrow)) %>%
  mutate(last_poll = map_dbl(data, ~max(.x$end_date)),
         last_poll = lubridate::as_date(last_poll)) %>%
  
  # model!
  mutate(model = map(data, poll_model)) %>%
  
  # predict current poll average
  nplyr::nest_filter(data, end_date == max(end_date)) %>%
  nplyr::nest_distinct(data, end_date, .keep_all = TRUE) %>%
  mutate(poll_avg = pmap_dbl(list(model, data), ~predict(..1, ..2))) %>%
  select(-data, -model) %>%
  
  # select the top two candidates by poll avg in each race
  group_by(cycle, race, state, seat_name) %>%
  arrange(desc(poll_avg)) %>%
  slice_head(n = 2) %>%
  ungroup()

# clean up environment
rm(polls_governor, polls_house, polls_senate)

# ------------------------------wrangle-gcb-------------------------------------

# function for getting the e-day gcb
gcb_model <- function(.data, begin_date, election_date) {
  
  .data %>%
    mutate(election_date = lubridate::mdy(election_date)) %>%
    select(cycle, 
           end_date,
           election_date,
           dem,
           rep) %>%
    pivot_longer(c(dem, rep),
                 names_to = "party",
                 values_to = "pct") %>%
    mutate(end_date = lubridate::mdy(end_date)) %>%
    filter(end_date >= lubridate::mdy(begin_date),
           end_date <= election_date) %>% 
    group_by(cycle, election_date, party) %>%
    nest() %>%
    ungroup() %>%
    mutate(model = map(data, ~loess(pct ~ as.numeric(end_date), data = .x)),
           pred_date = map_dbl(data, ~max(.x$end_date)),
           pred_date = lubridate::as_date(pred_date),
           gcb_avg = pmap_dbl(list(model, pred_date), ~predict(..1, newdata = ..2))) %>% 
    select(cycle, party, gcb_avg) %>%
    pivot_wider(names_from = party,
                values_from = gcb_avg)
  
}

# bind together
set.seed(888)
gcb <- 
  bind_rows(polls_gcb %>% gcb_model("1/1/17", "11/6/18"),
            polls_gcb %>% gcb_model("1/1/19", "11/3/20"))

# clean up environment
rm(polls_gcb)

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
  mutate(seat = replace_na(seat, "Governor"),
         candidate_party = str_to_lower(candidate_party),
         candidate_party = if_else(candidate_party %in% c("dem", "rep"), candidate_party, "ind")) %>%
  
  # get to only the top within each party
  group_by(cycle, race, state, seat, candidate_party) %>%
  arrange(desc(poll_avg)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  
  # sometimes dem/rep candidates are unevenly polled - summarise with the max
  group_by(cycle, race, state, seat) %>%
  mutate(num_polls = max(num_polls)) %>%
  ungroup() %>%
  
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
    "Montana", 
    "North Dakota", 
    "South Dakota", 
    "Vermont", 
    "Wyoming")

# prep training frame
elections_train <- 
  elections_mod %>%
  
  # append with polling average and mend NAs
  left_join(poll_averages, by = c("cycle", "race", "state", "seat")) %>%
  mutate(num_polls = replace_na(num_polls, 0),
         last_poll = replace_na(last_poll, max(poll_averages$last_poll))) %>%
  
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
  left_join(demographics, by = c("cycle" = "year", "join_region" = "region")) %>%
  select(-join_region) %>%
  
  # get dem2pv result/estimate
  mutate(result = dem_votes/(dem_votes + rep_votes),
         estimate = dem/(dem + rep)) %>%
  relocate(result, estimate, .after = rep_votes) %>%
  select(-ends_with("votes"), -dem, -rep) %>%
  
  # get incumbency
  mutate(incumbent = case_when(dem_incumbent == "y" & rep_incumbent == "n" ~ "dem",
                               dem_incumbent == "n" & rep_incumbent == "y" ~ "rep",
                               TRUE ~ "mix or neither")) %>%
  relocate(incumbent, .after = estimate) %>%
  select(-ends_with("_incumbent"), -prev_seat) %>%
    
  # get poll bucket/fix days
  mutate(poll_bucket = case_when(num_polls < 1 ~ "none",
                                 TRUE ~ "polled")) %>%
  mutate(last_poll = as.numeric(last_poll)) %>%
  
  # adjust racial groups to be on unbounded scale
  mutate(across(c(white:other), riekelib::logit)) %>%
  
  # remove unneeded cols
  select(-c(cycle:candidate_name_REP))

# -----------------------------------plots!-------------------------------------

elections_train %>%
  ggplot(aes(x = estimate,
             y = result,
             size = num_polls,
             color = incumbent)) + 
  geom_point(alpha = 0.25) +
  geom_abline() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_manual(values = c("blue", "purple", "red"))

elections_train %>%
  ggplot(aes(x = estimate,
             y = result,
             size = num_polls,
             color = poll_bucket)) +
  geom_point(alpha = 0.25) +
  geom_smooth(aes(size = NULL),
              method = "lm",
              se = FALSE) + 
  scale_size_continuous(range = c(1, 10))  +
  geom_abline()

elections_train %>%
  pivot_longer(c(white:other),
               names_to = "race_group",
               values_to = "race_pct") %>%
  ggplot(aes(x = race_pct,
             y = result,
             color = race_group,
             size = num_polls)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~race_group, scales = "free_x")

# -----------------------------------model!-------------------------------------

# estimate polling error
polling_error <- 
  elections_train %>%
  filter(poll_bucket == "polled") %>%
  mutate(err = result - estimate) %>%
  summarise(err_mean = mean(err),
            err_sd = sd(err)) %>%
  as.list()

# model !
set.seed(2022)
elections_model <- 
  gamlss(result ~ estimate*poll_bucket + incumbent + white + black + hispanic + aapi,
         sigma.formula = ~ log10(num_polls + 2),
         family = BE(),
         data = elections_train)

# --------------------------------diagnostics-----------------------------------

elections_model %>%
  summary()

predictions <- 
  tibble(mu = predict(elections_model, newdata = elections_train, what = "mu", type = "response"),
         sigma = predict(elections_model, newdata = elections_train, what = "sigma", type = "response")) %>%
  mutate(.pred = mu,
         .pred_lower = qBE(0.025, mu, sigma),
         .pred_upper = qBE(0.975, mu, sigma)) %>%
  bind_cols(elections_train) %>%
  mutate(within = if_else(result >= .pred_lower & result <= .pred_upper, "in", "out")) %>%
  select(starts_with(".pred"), result, num_polls, within) %>%
  bind_cols(elections_mod)

predictions %>%
  ggplot(aes(x = result,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper,
             size = num_polls,
             color = within)) +
  geom_point(alpha = 0.25) +
  geom_errorbar(aes(size = NULL),
                alpha = 0.75) +
  geom_abline(linetype = "dashed") +
  scale_size_continuous(range = c(1, 6)) +
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~race)

predictions %>%
  mutate(residual = .pred - result) %>%
  ggplot(aes(x = log10(num_polls + 1),
             y = abs(residual))) +
  geom_point()

# --------------------------------write-out-model-------------------------------

# punk november
elections_model %>% write_rds("models/punk_november.rds")
polling_error %>% write_rds("models/data/polling_error.rds")
poll_model %>% write_rds("models/data/poll_model.rds")  
gcb_model %>% write_rds("models/data/gcb_model.rds")
