# -----------------------------setup--------------------------------------------

# libraries
library(tidyverse)
library(tidymodels)
library(lubridate)
library(riekelib)

# data 
elections <-            read_csv("data/models/midterm_model/historical_results.csv")
variable_weights <-     read_csv("data/models/midterm_model/variable_weights.csv")
polls_train <-          read_csv("data/models/midterm_model/polls_train.csv")
region_similarities <-  read_csv("data/models/midterm_model/region_similarities.csv")
demographics <-         read_csv("data/models/midterm_model/demographics.csv")
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

# ----------------------------pre-train-formatting------------------------------

# format elections to just elections of interest (w/competition)
elections <- 
  elections %>%
  filter(if_all(starts_with("candidate"), ~ . != "-")) %>%
  select(cycle, race, state, seat, ends_with("votes"), ends_with("incumbent")) %>%
  mutate(dem2pv = dem_votes/(dem_votes + rep_votes),
         seat = paste(state, seat)) %>%
  select(-state, -ends_with("votes"))

# append training dataset with poll average
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

# append elections with demographics data
elections <- 
  elections %>%
  mutate(join_region = region,
         join_region = str_remove_all(join_region, " Governor"),
         join_region = str_remove_all(join_region, " Class III"),
         join_region = str_remove_all(join_region, " Class II"),
         join_region = str_remove_all(join_region, " Class I")) %>%
  left_join(demographics, by = c("cycle" = "year", "join_region" = "region")) %>%
  select(-join_region)

# --------------------------------model-----------------------------------------  

# set result to logit scale
elections <- 
  elections %>%
  mutate(result = logit(result))

# split into test/train
set.seed(999)
elections_split <- initial_split(elections, prop = 0.8, strata = race)
elections_train <- training(elections_split)
elections_test <- testing(elections_split)

# create resamples
set.seed(888)
elections_resamples <- vfold_cv(elections_train, strata = race)

# build out a preprocessing recipe
elections_rec <- 
  recipe(result ~ ., data = elections_train) %>%
  update_role(race, cycle, region, new_role = "id") %>%
  step_dummy(ends_with("incumbent")) 

# setup model spec
elections_spec <- 
  boost_tree(
    mode = "regression",
    mtry = tune(),
    sample_size = tune(),
    trees = tune()
  )

# combine in model workflow
elections_wf <-
  workflow() %>%
  add_recipe(elections_rec) %>%
  add_model(elections_spec) 

# setup tuning grid
set.seed(777)
elections_grid <-
  grid_latin_hypercube(
    finalize(mtry(), elections_rec %>% prep() %>% bake(new_data = NULL)),
    sample_size = sample_prop(),
    trees(),
    size = 25
  )

# tune hyperparameters
set.seed(666) # appropriate
elections_tuned <-
  tune_grid(
    elections_wf,
    resamples = elections_resamples,
    grid = elections_grid
  )

# collect metrics
elections_final <-
  elections_wf %>%
  finalize_workflow(elections_tuned %>% select_best("rmse"))

# --------------------------------save------------------------------------------

elections_final %>% write_rds("models/midterm_model.rds")

elections %>% 
  mutate(result = expit(result)) %>%
  write_rds("models/data/training.rds")
  

