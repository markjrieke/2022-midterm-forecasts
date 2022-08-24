# -----------------------------setup--------------------------------------------

# libraries
library(tidyverse)
library(workboots)

# load files
elections_model <- read_rds("models/midterm_model.rds")
training <- read_rds("models/data/training.rds")

# -----------------------------model-mean---------------------------------------

# format result
training <-
  training %>%
  mutate(result = riekelib::logit(result))

# split out data into analysis/assessment
set.seed(314)
boot_split <- 
  training %>% 
  rsample::bootstraps(times = 1, strata = race)

analysis <- boot_split$splits[[1]] %>% rsample::training()
assessment <- boot_split$splits[[1]] %>% rsample::testing()

# predictions - assessment
set.seed(918)
assessment_preds <-
  elections_model %>%
  predict_boots(
    n = 2000,
    training_data = analysis,
    new_data = assessment,
    verbose = TRUE
  )

# predictions - analysis
set.seed(314) 
analysis_preds <-
  elections_model %>%
  predict_boots(
    n = 2000,
    training_data = analysis,
    new_data = analysis,
    verbose = TRUE
  )

# -----------------------------model-variance-----------------------------------

# break out model training data
mod_analysis <- 
  analysis_preds %>%
  summarise_predictions() %>%
  select(.pred) %>%
  bind_cols(analysis) %>%
  mutate(resid = .pred - result) %>%
  select(n, resid)

mod_assessment <- 
  assessment_preds %>%
  summarise_predictions() %>%
  select(.pred) %>%
  bind_cols(assessment) %>%
  mutate(resid = .pred - result) %>%
  select(n, resid) %>%
  filter(abs(resid) < 2) 

# model oob
set.seed(651)
residual_oob <- 
  gamlss::gamlss(resid ~ 1,
                 sigma.formula = ~ log10(n + 1),
                 data = mod_assessment) 

# model trn
set.seed(7227)
residual_trn <-
  gamlss::gamlss(resid ~ 1,
                 sigma.formula = ~ log10(n + 1),
                 data = mod_analysis) 

# -----------------------------save---------------------------------------------

# models
residual_oob %>% write_rds("models/residual_oob.rds")
residual_trn %>% write_rds("models/residual_trn.rds")

# data to go along with models
mod_assessment %>% write_csv("models/data/mod_assessment.csv")
mod_analysis %>% write_csv("models/data/mod_analysis.csv")

