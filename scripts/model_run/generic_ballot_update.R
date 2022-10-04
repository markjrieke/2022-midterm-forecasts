# libraries ----
library(tidyverse)
library(lubridate)
library(furrr)
library(riekelib)
library(shadowtext)
library(ggtext)

# load fonts ----
extrafont::loadfonts(device = "win")

# cleanup ----
rm(list = ls())

# themes ----
theme_set(theme_minimal(base_family = "Roboto Slab") +
            theme(plot.background = element_rect(fill = "white", color = "white")))

# setup parallel processing (windows) ----
num_cores <- parallel::detectCores()
clusters <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(clusters)

# pull in data ----

# variable weights from training
variable_weights <- read_csv("data/models/generic_ballot/variable_weights.csv")

# vectors
pollsters <- read_rds("data/models/generic_ballot/pollsters.rds")
methods <- read_rds("data/models/generic_ballot/methods.rds")

# functions
generic_ballot_average <- read_rds("models/utils/generic_ballot/generic_ballot_average.rds")
pull_pollster_weights <- read_rds("models/utils/generic_ballot/pull_pollster_weights.rds")
pull_sample_weight <- read_rds("models/utils/generic_ballot/pull_sample_weight.rds")
pull_population_weights <- read_rds("models/utils/generic_ballot/pull_population_weights.rds")
pull_methodology_weights <- read_rds("models/utils/generic_ballot/pull_methodology_weights.rds")
pull_date_weight <- read_rds("models/utils/generic_ballot/pull_date_weight.rds")
pull_downweight <- read_rds("models/utils/generic_ballot/pull_downweight.rds")

# prep for fit ----

# pull in current generic ballot polls + wrangle
generic_2022 <-
  read_csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls.csv") %>%
  select(cycle, 
         display_name,
         sample_size,
         population_full,
         methodology,
         end_date,
         dem,
         rep,
         ind) %>%
  mutate(end_date = mdy(end_date),
         ind = replace_na(ind, 0),
         methodology = replace_na(methodology, "Unknown"),
         sample_size = round((dem + rep)/100 * sample_size),
         dem2pv = dem/(dem + rep),
         dem_votes = round(dem2pv * sample_size),
         rep_votes = round((1-dem2pv) * sample_size),
         population_full = str_replace(population_full, "lv-r", "lv")) %>%
  select(-dem, -rep, -ind, dem2pv) %>%
  drop_na() %>%
  rename(pollster = display_name) %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  drop_na()

# create sequence of dates up to 2022 election day
final_2022 <- seq(ymd("2021-04-01"), ymd("2022-11-08"), "days")
begin_2022 <- rep(ymd("2020-11-23"), length(final_2022))

# fit to election day ----

# setup futures
plan(multisession, workers = 8)

# fit
fit_2022 <- 
  list(begin = begin_2022,
       final = final_2022) %>%
  future_pmap_dfr(~generic_ballot_average(generic_2022,
                                          ..1,
                                          ..2,
                                          pull_pollster_weights(variable_weights),
                                          pull_sample_weight(),
                                          pull_population_weights(variable_weights),
                                          pull_methodology_weights(variable_weights),
                                          pull_date_weight(),
                                          pull_downweight()))

# plot ----

# store metbrewer colors 
rep_red <- MetBrewer::MetPalettes$Egypt[[1]][1]
dem_blu <- MetBrewer::MetPalettes$Egypt[[1]][2]

# get current dem/rep pct for plotting
current_dem_pct <- 
  fit_2022 %>%
  filter(date == Sys.Date()) %>%
  pull(dem2pv)

current_rep_pct <- 1 - current_dem_pct

# generate list of dates for which we have polls
plot_dates <-
  generic_2022 %>%
  distinct(end_date) %>%
  pull(end_date)

# reformat generic_2022 for plotting
generic_2022 <- 
  generic_2022 %>%
  select(end_date, pollster, dem2pv) %>%
  mutate(pollster = paste(pollster, "Offset")) %>%
  left_join(variable_weights, by = c("pollster" = "variable")) %>%
  select(-starts_with("next"), -search_suggestion) %>%
  mutate(dem2pv = dem2pv + weight) %>%
  select(-weight, -pollster) %>%
  mutate(rep2pv = 1 - dem2pv) %>%
  rename(date = end_date,
         dem_polls = dem2pv,
         rep_polls = rep2pv)

# plot
fit_2022 %>%
  mutate(across(dem2pv:ci_upper, ~if_else(date > Sys.Date(), as.double(NA), .x)),
         across(starts_with("ci"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA))),
         rep2pv = 1 - dem2pv,
         rep_ci_upper = 1 - ci_lower,
         rep_ci_lower = 1 - ci_upper) %>%
  left_join(generic_2022, by = "date") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = rep_ci_lower,
                  ymax = rep_ci_upper),
              fill = rep_red,
              alpha = 0.25,
              na.rm = TRUE) +
  geom_ribbon(aes(ymin = ci_lower,
                  ymax = ci_upper),
              fill = dem_blu,
              alpha = 0.25,
              na.rm = TRUE) +
  geom_point(aes(y = dem_polls),
             color = dem_blu,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_point(aes(y = rep_polls),
             color = rep_red,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_line(aes(y = rep2pv),
            color = "white",
            size = 3,
            na.rm = TRUE) +
  geom_line(aes(y = rep2pv),
            color = rep_red,
            size = 1.25,
            na.rm = TRUE) +
  geom_line(aes(y = dem2pv),
            color = "white",
            size = 3,
            na.rm = TRUE) +
  geom_line(aes(y = dem2pv),
            color = dem_blu,
            size = 1.25,
            na.rm = TRUE) +
  geom_vline(xintercept = Sys.Date(),
             size = 1,
             linetype = "dotted",
             color = "gray",
             na.rm = TRUE) +
  geom_shadowtext(x = Sys.Date() + 40,
                  y = current_rep_pct,
                  label = paste0(round(current_rep_pct, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = rep_red,
                  bg.color = "white") +
  geom_shadowtext(x = Sys.Date() + 40,
                  y = current_dem_pct,
                  label = paste0(round(current_dem_pct, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = dem_blu,
                  bg.color = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracty = 1)) +
  scale_x_date(labels = scales::date_format("%b"),
               breaks = "month") +
  theme(panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(family = "Roboto Slab")) +
  labs(title = "Do Voters Want <span style=color:'#0F7BA2'>**Democrats**</span> or <span style=color:'#DD5129'>**Republicans**</span> in Congress?",
       subtitle = paste("Estimated two-party voteshare of the generic congressional ballot as of",
                        format(Sys.Date(), "%b %d")),
       x = NULL,
       y = NULL,
       caption = paste0("Model by @markjrieke\n",
                        "Data courtesy of @FiveThirtyEight\n",
                        "https://projects.fivethirtyeight.com/congress-generic-ballot-polls/")) +
  expand_limits(x = c(final_2022, ymd("2022-12-31")))

# save over current photo
ggsave("plots/generic_ballot/generic_ballot_current.png",
       device = png,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)
