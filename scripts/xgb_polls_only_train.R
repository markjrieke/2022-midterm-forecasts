#################### Post-poll XGB test ####################
#
# This script is just to check that XGB can eek out meaningful signal 
# based on polls + some really basic info (i.e., just incumbency)
#
#

# libraries ----
library(tidyverse)
library(tidymodels)
library(lubridate)
library(riekelib)
library(xgboost)

# set theme
extrafont::loadfonts(device = "win")
theme_set(theme_minimal(base_family = "Roboto Slab") + 
            theme(plot.background = element_rect(fill = "white", color = "white")))

# read in data/functions ----

# data 
elections <-            read_csv("data/models/midterm_model/historical_results.csv")
variable_weights <-     read_csv("data/models/midterm_model/variable_weights.csv")
polls_train <-          read_csv("data/models/midterm_model/polls_train.csv")
region_similarities <-  read_csv("data/models/midterm_model/region_similarities.csv")
methods <-              read_rds("data/models/midterm_model/methods.rds")

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

# format elections to just elections of interest (w/competition) ----
elections <- 
  elections %>%
  filter(across(starts_with("candidate"), ~ .x != "-")) %>%
  select(cycle, race, state, seat, ends_with("votes"), ends_with("incumbent")) %>%
  mutate(dem2pv = dem_votes/(dem_votes + rep_votes),
         seat = paste(state, seat)) %>%
  select(-state, -ends_with("votes"))

# append training dataset with poll average ----

# create list to map over passer function
elections_list <- 
  elections %>%
  select(race, 
         cycle, 
         region = seat) %>%
  mutate(begin_date = if_else(cycle == 2018, ymd("2016-11-04"), ymd("2018-11-07")),
         end_date = if_else(cycle == 2018, ymd("2018-11-06"), ymd("2020-11-03"))) %>%
  as.list()

# return polling results (WASSP = Weighted Average of Similarity-Scored Polls)
wassp_average <- 
  elections_list %>%
  pmap_dfr(~pass_current_fit(polls_train, ..1, ..2, ..3, ..4, ..5))

# append wassp with info from elections_list (for binding back to historical results)
elections <- 
  wassp_average %>%
  bind_cols(race = elections_list$race,
            cycle = elections_list$cycle,
            region = elections_list$region) %>%
  left_join(elections, by = c("cycle", "race", "region" = "seat")) %>%
  rename(estimate = dem2pv.x,
         result = dem2pv.y) %>%
  relocate(race, cycle, region, result) %>%
  select(-date) 

# eda ----

# fn for saving eda
save_eda <- function(filename) {
  
  ggsave(paste0("plots/midterm_forecast/xgb_polls_only/",
                filename,
                ".png"),
         width = 9,
         height = 6,
         units = "in",
         dpi = 500)
  
}

# pretty normal
elections %>%
  ggplot(aes(x = result)) +
  geom_histogram()

save_eda("eda_01")

# transform gives non-normal result; maybe deal with via yeo-johnson? or just not transform
elections %>%
  mutate(result = logit(result)) %>%
  ggplot(aes(x = result)) +
  geom_histogram()

save_eda("eda_02")

# hmmmm..... explore normalization techniques on the result...
elections %>%
  mutate(result = logit(result)) %>%
  filter(result < 5) %>%
  ggplot(aes(x = result)) +
  geom_histogram()

save_eda("eda_03")

# relatively normal across all three races
elections %>%
  mutate(result = logit(result)) %>%
  filter(result < 5) %>%
  ggplot(aes(x = result)) +
  geom_density() +
  facet_wrap(~race, scales = "free_y")

save_eda("eda_04")

# lots that are just 0.5
elections %>%
  ggplot(aes(x = estimate)) +
  geom_histogram()

save_eda("eda_05")

# logit transform is pretty perfect on the estimate
elections %>%
  mutate(estimate = logit(estimate)) %>%
  ggplot(aes(x = estimate)) + 
  geom_histogram()

save_eda("eda_06")

# obviously a way bigger spike at 50%/0 on logit for house - maybe an interaction is appropriate?
elections %>%
  mutate(estimate = logit(estimate)) %>%
  ggplot(aes(x = estimate)) + 
  geom_density() +
  facet_wrap(~race, scales = "free_y")

save_eda("eda_07")

# similar to before - higher polled areas are closer to the actual results
elections %>%
  ggplot(aes(x = estimate,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10))

save_eda("eda_08")

# some signal here  
elections %>%
  ggplot(aes(x = ci_lower,
             y = result)) + 
  geom_point()

save_eda("eda_09")

# signal here, makes sense
elections %>%
  ggplot(aes(x = ci_upper,
             y = result)) +
  geom_point()

save_eda("eda_10")

# almost looks like there are two separate "paths"
elections %>%
  mutate(range = ci_upper - ci_lower) %>%
  ggplot(aes(x = range,
             y = result)) + 
  geom_point()

save_eda("eda_11")

# definitely need an interaction between range & incumbency
elections %>%
  mutate(range = ci_upper - ci_lower) %>%
  ggplot(aes(x = range,
             y = result,
             color = dem_incumbent)) + 
  geom_point()

save_eda("eda_12")

# see above
elections %>%
  mutate(range = ci_upper - ci_lower) %>%
  ggplot(aes(x = range,
             y = result,
             color = rep_incumbent)) + 
  geom_point()

save_eda("eda_13")

# similarly want to add interactions for incumbency here
elections %>%
  ggplot(aes(x = estimate,
             y = result,
             size = n,
             color = dem_incumbent)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10)) +
  facet_wrap(~race)

save_eda("eda_14")

# as expected, there's a pretty hefty incumbent advantage
elections %>%
  ggplot(aes(x = dem_incumbent,
             y = result,
             color = dem_incumbent)) +
  geom_boxplot() +
  geom_point(aes(size = n),
             position = position_jitter(),
             alpha = 0.25)

save_eda("eda_15")

# n is important too - pushes races closer to 50%
elections %>%
  ggplot(aes(x = n,
             y = result,
             color = dem_incumbent)) +
  geom_point(alpha = 0.25)

save_eda("eda_16")

elections %>%
  ggplot(aes(x = n)) +
  geom_histogram()

save_eda("eda_17")

elections %>%
  ggplot(aes(x = n)) +
  geom_density() +
  facet_wrap(~race, scales = "free")

save_eda("eda_18")

# modeltime! ----

# split
set.seed(123)
elections_split <- elections %>% initial_split(prop = 0.8, strata = race)
elections_train <- elections_split %>% training()
elections_test <- elections_split %>% testing()

# create resamples
elections_boot <- elections_train %>% bootstraps(strata = race)

# basic training
basic_rec <- 
  recipe(result ~ ., data = elections_train) %>%
  update_role(race, cycle, region, new_role = "id") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)
  
basic_spec <- 
  boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")

basic_fit <- 
  workflow() %>%
  add_recipe(basic_rec) %>%
  add_model(basic_spec) %>%
  fit(data = elections_train)

basic_fit %>%
  predict(new_data = elections_train) %>%
  bind_cols(elections_train) %>%
  ggplot(aes(x = .pred,
             y = result)) +
  geom_point(alpha = 0.25)

save_eda("fit_01")

# note: good fit, now try adding weight by n to model

basic_wt_spec <-
  boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost",
             weight = elections_train$n)

basic_wt_fit <-
  workflow() %>%
  add_recipe(basic_rec) %>%
  add_model(basic_wt_spec) %>%
  fit(data = elections_train)

basic_wt_fit %>%
  predict(new_data = elections_train) %>%
  bind_cols(elections_train) %>%
  ggplot(aes(x = .pred,
             y = result)) +
  geom_point(alpha = 0.25)

# note: tidymodels currently doesn't support weights!
rm(basic_wt_fit, basic_wt_spec)

# let's try w/xgb native
basic_wt_fit <- 
  xgboost(basic_rec %>%
            prep() %>%
            bake(new_data = NULL) %>%
            select(-(race:region), -result) %>%
            as.matrix(),
          nrounds = 100,
          label = elections_train$result,
          weight = elections_train %>% mutate(n = n + 1) %>% pull(n))

basic_wt_fit %>%
  predict(basic_rec %>%
            prep() %>%
            bake(new_data = NULL) %>%
            select(-(race:region), -result) %>%
            as.matrix) %>%
  as_tibble() %>%
  rename(.pred = value) %>%
  bind_cols(elections_train) %>%
  ggplot(aes(x = .pred,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10))

# WOW!!!!!!!

save_eda("fit_02")

basic_wt_fit %>%
  predict(basic_rec %>%
            prep() %>%
            bake(new_data = NULL) %>%
            select(-(race:region), -result) %>%
            as.matrix) %>%
  as_tibble() %>%
  rename(.pred = value) %>%
  bind_cols(elections_train) %>%
  ggplot(aes(x = .pred,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10)) +
  facet_wrap(~race)

save_eda("fit_02_facet")

basic_wt_fit %>%
  predict(basic_rec %>%
            prep() %>%
            bake(new_data = elections_test) %>%
            select(-(race:region), -result) %>%
            as.matrix) %>%
  as_tibble() %>%
  rename(.pred = value) %>%
  bind_cols(elections_test) %>%
  ggplot(aes(x = .pred,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10)) +
  facet_wrap(~race)

save_eda("fit_02_test_facet")

# debug ----
