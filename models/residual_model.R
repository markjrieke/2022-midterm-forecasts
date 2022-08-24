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
training_split <- rsample::initial_split(training, prop = 0.8, strata = race)
analysis <- training_split %>% rsample::training()
assessment <- training_split %>% rsample::assessment()

# predictions
set.seed(918)
elections_preds <-
  elections_model %>%
  predict_boots(
    n = 2000,
    training_data = analysis,
    new_data = training,
    verbose = TRUE
  )

# -----------------------------model-variance-----------------------------------

# append training frame w/predictions
training <- 
  elections_preds %>%
  summarise_predictions() %>%
  select(.pred) %>%
  bind_cols(training)

# split out into analysis/assessment
assessment <- 
  training %>%
  anti_join(analysis, by = c("region", "cycle"))

analysis <- 
  training %>%
  anti_join(assessment, by = c("region", "cycle"))

# prep oob model
mod_assessment <- 
  assessment %>%
  mutate(resid = .pred - result) %>%
  select(n, resid) %>%
  filter(abs(resid) < 2,
         n < 75)

# model
set.seed(651)
residual_oob <- 
  gamlss::gamlss(resid ~ 1,
                 sigma.formula = ~ log10(n + 1),
                 data = mod_assessment) 

# prep trn model
mod_analysis <-
  analysis %>%
  mutate(resid = .pred - result) %>%
  select(n, resid)

# model
set.seed(7227)
residual_trn <-
  gamlss::gamlss(resid ~ 1,
                 sigma.formula = ~ log10(n + 1),
                 data = mod_analysis) 

# -----------------------------save---------------------------------------------

residual_oob %>% write_rds("models/residual_oob.rds")
residual_trn %>% write_rds("models/residual_trn.rds")


