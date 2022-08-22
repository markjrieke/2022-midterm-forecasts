# libraries ----
library(tidyverse)

# read in data ----
demographics <- read_csv("models/data/demographics_2022.csv")

# determine similarity scores ----

# function for calculating similarity scores given one congressional district
similarity <- function(.data, input_region) {
  
  message(paste("Similarities for", input_region))
  
  .data %>%
    mutate(comparison = input_region) %>% 
    left_join(demographics, by = c("comparison" = "region")) %>%
    mutate(across(where(is.numeric), riekelib::logit),
           white_similar = exp(-((white.y - white.x)^2)/sd(white.x)),
           black_similar = exp(-((black.y - black.x)^2)/sd(black.x)),
           hispanic_similar = exp(-((hispanic.y - hispanic.x)^2)/sd(hispanic.x)),
           aapi_similar = exp(-((aapi.y - aapi.x)^2)/sd(aapi.x)),
           other_similar = exp(-((other.y - other.x)^2)/sd(other.x)),
           similarity = white_similar * black_similar * hispanic_similar * aapi_similar * other_similar) %>%
    select(region, comparison, similarity)
  
}

similarities <-
  demographics %>%
  pull(region) %>%
  map_dfr(~similarity(demographics, .x))

similarities %>%
  mutate(year = 2022) %>%
  relocate(year) %>%
  write_csv("models/data/similarities_2022.csv")
