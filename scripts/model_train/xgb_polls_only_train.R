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

# format elections to just elections of interest (w/competition) ----
elections <- 
  elections %>%
  filter(if_all(starts_with("candidate"), ~ . != "-")) %>%
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

# explore races
elections %>%
  pivot_longer(c(white, black, hispanic, aapi, other),
               names_to = "race_group",
               values_to = "pct") %>%
  ggplot(aes(x = pct, 
             y = result,
             size = n,
             color = race_group)) +
  geom_point(alpha = 0.15) +
  facet_wrap(~race_group, scales = "free_x")

save_eda("eda_19")

elections %>%
  pivot_longer(c(white, black, hispanic, aapi, other),
               names_to = "race_group",
               values_to = "pct") %>%
  ggplot(aes(x = pct,
             y = result,
             size = n,
             color = race_group)) + 
  geom_point(alpha = 0.15) +
  facet_wrap(~race_group, scales = "free_x") +
  geom_smooth(method = "lm",
              formula = y ~ splines::ns(x, df = 5),
              se = FALSE)

save_eda("eda_20")

elections %>%
  pivot_longer(c(white, black, hispanic, aapi, other),
               names_to = "race_group",
               values_to = "pct") %>%
  mutate(pct = logit(pct)) %>%
  ggplot(aes(x = pct,
             y = result,
             size = n,
             color = race_group)) + 
  geom_point(alpha = 0.15) +
  facet_wrap(~race_group, scales = "free_x") +
  geom_smooth(method = "lm",
              formula = y ~ splines::ns(x, df = 4),
              se = FALSE)

save_eda("eda_21")

# baseline/basic models ----

# split
set.seed(123)
elections_split <- elections %>% initial_split(prop = 0.8, strata = race)
elections_train <- elections_split %>% training()
elections_test <- elections_split %>% testing()

# create resamples
set.seed(234)
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

set.seed(456)
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

set.seed(567)
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
set.seed(678)
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

# pretty much just *learned* the training dataset - need to tune!
# what I'll do for now is modify the recipe for working on feature engineering
# and just set the subsample in the model

# setup some functions for wrapping around xgboost ----

# pass recipe & format as matrix
prep_xgb <- function(recipe, training) {
  
  recipe %>%
    prep() %>%
    bake(new_data = training) %>%
    select(-(race:region), -result) %>%
    as.matrix()
  
}

# train xgboost model
train_xgb <- function(recipe,
                      training, 
                      mtry_prop = 1, 
                      sample_prop = 1, 
                      trees = 15,
                      ...) {
  
  xgb_matrix <-
    prep_xgb(recipe, training)
  
  xgb_labels <-
    training %>% pull(result)
  
  xgb_weights <-
    training %>%
    mutate(n = n + 1) %>%
    pull(n)
  
  xgb_model <- 
    xgboost(data = xgb_matrix,
            label = xgb_labels,
            weight = xgb_weights,
            nrounds = trees,
            subsample = sample_prop,
            colsample_bynode = mtry_prop,
            ...)
  
  return(xgb_model)
  
}

# predict xgboost model & bind df
predict_xgb <- function(xgb_model, recipe, new_data) {
  
  xgb_model %>%
    predict(prep_xgb(recipe, new_data)) %>%
    as_tibble() %>%
    rename(.pred = value) %>%
    bind_cols(new_data)
  
}

# build final model recipe ----

# pull out one of the train/test boots
xgb_cv_train <- elections_boot$splits[[15]] %>% training()
xgb_cv_test <- elections_boot$splits[[15]] %>% testing()  

# clean up environment 
rm(basic_fit, basic_rec, basic_spec, basic_wt_fit)

# build out recipe & test on an one of the boots
xgb_rec <- 
  recipe(result ~ ., data = elections_train) %>%
  update_role(race, cycle, region, new_role = "id") %>% 
  step_dummy(ends_with("incumbent"), one_hot = TRUE) %>%
  step_mutate(estimate = riekelib::logit(estimate),
              ci_lower = riekelib::logit(ci_lower),
              ci_upper = riekelib::logit(ci_upper),
              ci_range = ci_upper - ci_lower,
              white = riekelib::logit(white),
              black = riekelib::logit(black),
              hispanic = riekelib::logit(hispanic),
              aapi = riekelib::logit(aapi),
              other = riekelib::logit(other)) %>%
  step_ns(white, black, hispanic, aapi, other, deg_free = 4) %>%
  step_interact(~estimate:starts_with("dem")) %>%
  step_interact(~estimate:starts_with("rep")) %>%
  step_interact(~starts_with("dem"):starts_with("ci")) %>%
  step_interact(~starts_with("rep"):starts_with("ci")) %>%
  step_mutate(n_inv = 1/(n+1))

# check recipe functions correctly
xgb_rec %>% 
  prep() %>% 
  bake(new_data = NULL)

# append cv df with results
xgb_res <- 
  xgb_rec %>%
  train_xgb(xgb_cv_train) %>%
  predict_xgb(xgb_rec, xgb_cv_test)

# plot
xgb_res %>%
  ggplot(aes(x = .pred,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10))

# check rmse
xgb_res %>%
  rmse(truth = result,
       estimate = .pred)

# tune xgb model ----

# define tuning function 
tune_xgb <- function(recipe, 
                     train_data, 
                     test_data,
                     mtry_prop,
                     sample_prop,
                     trees) {
  
  recipe %>%
    train_xgb(train_data,
              mtry_prop,
              sample_prop,
              trees) %>%
    predict_xgb(recipe, 
                test_data) %>%
    mutate(mtry_prop = mtry_prop,
           sample_prop = sample_prop,
           trees = trees) %>%
    group_by(mtry_prop, sample_prop, trees) %>%
    nest() %>%
    ungroup() %>%
    mutate(rmse = map(data, rmse, truth = result, estimate = .pred)) %>%
    unnest(rmse) %>%
    select(-.metric, -.estimator) %>%
    rename(rmse = .estimate)
  
  
}

# function for grid creation
create_grid <- function(mtry_range = c(1L, 47),
                        sample_prop_range = c(0.1, 1),
                        trees_range = c(1L, 200),
                        size = 10,
                        seed = 123) {
  
  # create grid for tuning
  set.seed(seed)
  entropy_grid <- 
    grid_max_entropy(
      finalize(mtry(mtry_range)),
      sample_prop(sample_prop_range),
      trees(trees_range),
      size = size
    )
  
  # modify for passing mtry
  entropy_grid <- 
    entropy_grid %>%
    mutate(mtry = mtry/47)
  
  # modify entropy grid for passing bootstrap
  entropy_grid <- 
    entropy_grid %>%
    mutate(boot_01 = 1,
           boot_02 = 2,
           boot_03 = 3,
           boot_04 = 4,
           boot_05 = 5,
           boot_06 = 6,
           boot_07 = 7,
           boot_08 = 8,
           boot_09 = 9,
           boot_10 = 10,
           boot_11 = 11,
           boot_12 = 12,
           boot_13 = 13,
           boot_14 = 14,
           boot_15 = 15,
           boot_16 = 16,
           boot_17 = 17,
           boot_18 = 18,
           boot_19 = 19,
           boot_20 = 20,
           boot_21 = 21,
           boot_22 = 22,
           boot_23 = 23,
           boot_24 = 24,
           boot_25 = 25) %>%
    pivot_longer(starts_with("boot"),
                 names_to = "boot_id",
                 values_to = "index") %>%
    relocate(boot_id, index)
  
  return(entropy_grid)
  
}

# passer function for passing to tune_xgb
pass_tuner <- function(recipe,
                       boot_id,
                       index,
                       mtry,
                       sample_size,
                       trees) {
  
  xgb_cv_train <- elections_boot$splits[[index]] %>% training()
  xgb_cv_test <- elections_boot$splits[[index]] %>% testing()
  
  tune_results <- 
    tune_xgb(recipe,
             xgb_cv_train,
             xgb_cv_test,
             mtry_prop = mtry,
             sample_prop = sample_size,
             trees = trees) %>%
    mutate(boot_id = boot_id) %>%
    relocate(boot_id)
  
  return(tune_results)
  
}

# ooh boy here we go ----

# setup parallel processing (Windows) 
num_cores <- parallel::detectCores()
clusters <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(clusters)

# setup futures for furrr
future::plan("multisession", workers = 8)

# create grid
entropy_grid <- create_grid()

# map to furrr
tuned_xgb <-
  entropy_grid %>%
  as.list() %>%
  furrr::future_pmap_dfr(~pass_tuner(xgb_rec, ..1, ..2, ..3, ..4, ..5))

# check results
tuned_xgb %>%
  group_by(mtry_prop, sample_prop, trees) %>%
  summarise(rmse = mean(rmse)) %>%
  arrange(desc(rmse)) %>%
  ungroup() %>%
  pivot_longer(cols = c(mtry_prop, sample_prop, trees),
               names_to = "metric",
               values_to = "value") %>%
  ggplot(aes(x = value,
             y = rmse,
             color = metric)) +
  geom_point() +
  facet_wrap(~metric, scales = "free_x")

save_eda("tune_01")

# looks like best mtry is between 20-60%
# best sample size is between 75-100%
# number of trees is all over the place - check higher range

# create grid
entropy_grid <-
  create_grid(
    mtry_range = c(10, 28),
    sample_prop_range = c(0.75, 1),
    trees_range = c(1L, 1000),
    size = 10,
    seed = 456
  )

# map to furrr
tuned_xgb <-
  entropy_grid %>%
  as.list() %>%
  furrr::future_pmap_dfr(~pass_tuner(xgb_rec, ..1, ..2, ..3, ..4, ..5))

# check results
tuned_xgb %>%
  group_by(mtry_prop, sample_prop, trees) %>%
  summarise(rmse = mean(rmse)) %>%
  arrange(desc(rmse)) %>%
  ungroup() %>%
  pivot_longer(cols = c(mtry_prop, sample_prop, trees),
               names_to = "metric",
               values_to = "value") %>%
  ggplot(aes(x = value,
             y = rmse,
             color = metric)) +
  geom_point() +
  facet_wrap(~metric, scales = "free_x")

save_eda("tune_02")

# one more round of tuning
# mtry_prop sees the best let's say between 0.2 & 0.25
# sample_prop between 0.9 & 1
# trees, maybe between 750/1000?

# create grid
entropy_grid <-
  create_grid(
    mtry_range = c(5, 15),
    sample_prop_range = c(0.9, 1),
    trees_range = c(750, 1500),
    size = 10,
    seed = 567
  )

# map to furrr
tuned_xgb <-
  entropy_grid %>%
  as.list() %>%
  furrr::future_pmap_dfr(~pass_tuner(xgb_rec, ..1, ..2, ..3, ..4, ..5))

# check results
tuned_xgb %>%
  group_by(mtry_prop, sample_prop, trees) %>%
  summarise(rmse = mean(rmse)) %>%
  arrange(desc(rmse)) %>%
  ungroup() %>%
  pivot_longer(cols = c(mtry_prop, sample_prop, trees),
               names_to = "metric",
               values_to = "value") %>%
  ggplot(aes(x = value,
             y = rmse,
             color = metric)) +
  geom_point() +
  facet_wrap(~metric, scales = "free_x")

# could go further, but I think I'm getting diminishing returns at this point.

save_eda("tune_03")

# final hyperparameter selection & fit ----

# get final parameters
final_parameters <- 
  tuned_xgb %>%
  group_by(mtry_prop, sample_prop, trees) %>%
  summarise(rmse = mean(rmse)) %>%
  ungroup() %>%
  filter(rmse == min(rmse)) %>%
  select(-rmse)

# final fit on train/test
set.seed(2022)
final_fit <- 
  final_parameters %>%
  as.list() %>%
  pmap_dfr(~tune_xgb(xgb_rec, 
                     elections_train,
                     elections_test,
                     ..1,
                     ..2,
                     ..3))

# plot
final_fit %>%
  select(data) %>%
  unnest(data) %>%
  ggplot(aes(x = .pred,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10))

save_eda("tune_04")

final_fit %>%
  select(data) %>%
  unnest(data) %>%
  ggplot(aes(x = .pred,
             y = result,
             size = n)) +
  geom_point(alpha = 0.25) +
  scale_size_continuous(range = c(1, 10)) +
  facet_wrap(~race)

save_eda("tune_05")

# what are the important features?
set.seed(2022)
train_xgb(xgb_rec,
          elections_train,
          final_parameters$mtry_prop,
          final_parameters$sample_prop,
          final_parameters$trees) %>%
  vip::vi() %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Variable,
             y = Importance)) + 
  geom_point() +
  coord_flip()

save_eda("tune_06")

# note that the variable importance varies quite a bit 
# depending on the seed - for final proj use the variable importance
# based on mean & spread across all models 


# debug ----
