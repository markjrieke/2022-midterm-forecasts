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
approval_weights <- read_csv("data/models/approval/approval_weights.csv")
disapproval_weights <- read_csv("data/models/approval/disapproval_weights.csv")

# vectors
pollsters <- read_rds("data/models/approval/pollsters.rds")
methods <- read_rds("data/models/approval/methods.rds")

# functions
approval_average <- read_rds("models/utils/approval/approval_average.rds")
pull_pollster_weights <- read_rds("models/utils/approval/pull_pollster_weights.rds")
pull_sample_weight <- read_rds("models/utils/approval/pull_sample_weight.rds")
pull_population_weights <- read_rds("models/utils/approval/pull_population_weights.rds")
pull_methodology_weights <- read_rds("models/utils/approval/pull_methodology_weights.rds")
pull_date_weight <- read_rds("models/utils/approval/pull_date_weight.rds")
pull_downweight <- read_rds("models/utils/approval/pull_downweight.rds")

# prep for fit ----

# pull in & wrangle current polls
approval_polls_current <-
  read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_approval_polls.csv") %>%
  select(display_name,
         sample_size,
         population_full,
         methodology,
         end_date,
         yes,
         no) %>%
  mutate(end_date = mdy(end_date),
         methodology = replace_na(methodology, "Unknown"),
         yes = yes/100,
         no = no/100) %>%
  rename(pollster = display_name) %>%
  mutate(pollster = if_else(pollster %in% pollsters, pollster, "Other Pollster"),
         methodology = if_else(methodology %in% methods, methodology, "Other Method")) %>%
  drop_na()


# create a sequence of days to fit against
final_2022 <- seq(ymd("2021-01-23"), ymd("2022-11-08"), "days")
begin_2022 <- rep(ymd("2021-01-21"), length(final_2022))

# fit to election day ----

# setup futures
plan(multisession, workers = 8)

# fit approval
fit_2022_approval <-
  list(begin = begin_2022,
       final = final_2022) %>%
  future_pmap_dfr(~approval_average(approval_polls_current,
                                    ..1, 
                                    ..2,
                                    pull_pollster_weights(approval_weights),
                                    pull_sample_weight(approval_weights),
                                    pull_population_weights(approval_weights),
                                    pull_methodology_weights(approval_weights),
                                    pull_date_weight(approval_weights),
                                    yes,
                                    pull_downweight("approval")))

# fit disapproval
fit_2022_disapproval <-
  list(begin = begin_2022,
       final = final_2022) %>%
  future_pmap_dfr(~approval_average(approval_polls_current,
                                    ..1, 
                                    ..2,
                                    pull_pollster_weights(disapproval_weights),
                                    pull_sample_weight(disapproval_weights),
                                    pull_population_weights(disapproval_weights),
                                    pull_methodology_weights(disapproval_weights),
                                    pull_date_weight(disapproval_weights),
                                    no,
                                    pull_downweight("disapproval")))

# merge frames
fit_2022 <- 
  fit_2022_approval %>%
  left_join(fit_2022_disapproval, by = "date") %>%
  rename(approval = answer.x,
         approval_lo = ci_lower.x,
         approval_hi = ci_upper.x,
         disapproval = answer.y,
         disapproval_lo = ci_lower.y,
         disapproval_hi = ci_upper.y)

# get current approval/disapproval estimates
current_approval <-
  fit_2022 %>%
  filter(date == Sys.Date()) %>%
  pull(approval)

current_disapproval <-
  fit_2022 %>%
  filter(date == Sys.Date()) %>%
  pull(disapproval)

current_net <- current_approval - current_disapproval  

# generate list of dates for which we have polls
plot_dates <-
  approval_polls_current %>%
  distinct(end_date) %>%
  pull(end_date)

# reformat approval_polls_current for plotting
approval_polls_current <-
  approval_polls_current %>%
  select(end_date, pollster, yes, no) %>%
  mutate(pollster = paste(pollster, "Offset")) %>%
  left_join(approval_weights, by = c("pollster" = "variable")) %>%
  mutate(yes = yes + weight) %>%
  select(end_date:no) %>%
  left_join(disapproval_weights, by = c("pollster" = "variable")) %>%
  mutate(no = no + weight) %>%
  select(end_date:no)

# set colors from metbrewer
dis_org <- MetBrewer::MetPalettes$Hiroshige[[1]][2]
app_blu <- MetBrewer::MetPalettes$Hiroshige[[1]][7]
net_pur <- MetBrewer::MetPalettes$Signac[[1]][10]
  
# plot approval/disapproval ----
fit_2022 %>%
  mutate(across(approval:disapproval_hi, ~if_else(date > Sys.Date(), as.double(NA), .x)),
         across(ends_with("lo"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA))),
         across(ends_with("hi"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA)))) %>%
  left_join(approval_polls_current, by = c("date" = "end_date")) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0.5,
             size = 1,
             color = "gray") +
  geom_ribbon(aes(ymin = disapproval_lo,
                  ymax = disapproval_hi),
              fill = dis_org,
              alpha = 0.25,
              na.rm = TRUE) +
  geom_ribbon(aes(ymin = approval_lo,
                  ymax = approval_hi),
              fill = app_blu,
              alpha = 0.25,
              na.rm = TRUE) +
  geom_point(aes(y = yes),
             color = app_blu,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_point(aes(y = no),
             color = dis_org,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_line(aes(y = disapproval),
            color = "white",
            size = 3,
            na.rm = TRUE) +
  geom_line(aes(y = disapproval),
            color = dis_org,
            size = 1.25,
            na.rm = TRUE) +
  geom_line(aes(y = approval),
            color = "white",
            size = 3,
            na.rm = TRUE) +
  geom_line(aes(y = approval),
            color = app_blu,
            size = 1.25,
            na.rm = TRUE) +
  geom_vline(xintercept = Sys.Date(),
             size = 1,
             linetype = "dotted",
             color = "gray") +
  geom_shadowtext(x = Sys.Date() + 45,
                  y = current_disapproval,
                  label = paste0(round(current_disapproval, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = dis_org,
                  bg.color = "white") +
  geom_shadowtext(x = Sys.Date() + 45,
                  y = current_approval,
                  label = paste0(round(current_approval, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = app_blu,
                  bg.color = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.25, 0.75)) +
  scale_x_date(labels = scales::date_format("%b"),
               breaks = "2 months") +
  theme(panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(family = "Roboto Slab")) +
  labs(title = "Do Voters <span style=color:'#72BCD5'>**Approve**</span> or <span style=color:'#EF8A47'>**Disapprove**</span> of Joe Biden's Performance?",
       subtitle = paste("Estimated presidential approval and disapproval as of",
                        format(Sys.Date(), "%b %d")),
       x = NULL,
       y = NULL,
       caption = paste0("Model by @markjrieke\n",
                        "Data courtesy of @FiveThirtyEight\n",
                        "https://projects.fivethirtyeight.com/biden-approval-rating/")) +
  expand_limits(x = c(final_2022, ymd("2022-12-31")))

ggsave("plots/approval/approval_disapproval_current.png",
       device = png,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)  

# plot net approval ----
fit_2022 %>%
  mutate(across(approval:disapproval_hi, ~if_else(date > Sys.Date(), as.double(NA), .x)),
         across(ends_with("lo"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA))),
         across(ends_with("hi"), ~if_else(date %in% plot_dates | date >= Sys.Date() | date == min(date), .x, as.double(NA)))) %>%
  left_join(approval_polls_current, by = c("date" = "end_date")) %>%
  mutate(net = approval - disapproval,
         net_lo = approval_lo - disapproval_hi,
         net_hi = approval_hi - disapproval_lo,
         net_poll = yes - no) %>%
  select(date, starts_with("net")) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0,
             size = 1,
             color = "gray") +
  geom_ribbon(aes(ymin = net_lo,
                  ymax = net_hi),
              fill = net_pur,
              alpha = 0.25,
              na.rm = TRUE) +
  geom_point(aes(y = net_poll),
             color = net_pur,
             size = 2,
             alpha = 0.25,
             na.rm = TRUE) +
  geom_line(aes(y = net),
            color = "white",
            size = 3,
            na.rm = TRUE) +
  geom_line(aes(y = net),
            color = net_pur,
            size = 1.25,
            na.rm = TRUE) +
  geom_vline(xintercept = Sys.Date(),
             size = 1,
             linetype = "dotted",
             color = "gray") +
  geom_shadowtext(x = Sys.Date() + 45,
                  y = current_net,
                  label = paste0(round(current_net, 3) * 100, "%"),
                  size = 8,
                  family = "Roboto Slab",
                  color = net_pur,
                  bg.color = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.4, 0.4)) +
  scale_x_date(labels = scales::date_format("%b"),
               breaks = "2 months") +
  theme(panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(family = "Roboto Slab")) +
  labs(title = "How <span style=color:'#9F5691'>**Popular**</span> is Joe Biden?",
       subtitle = paste("Estimated presidential net approval as of", 
                        format(Sys.Date(), "%b %d")),
       x = NULL,
       y = NULL, 
       caption = paste0("Model by @markjrieke\n",
                        "Data courtesy of @FiveThirtyEight\n",
                        "https://projects.fivethirtyeight.com/biden-approval-rating/")) +
  expand_limits(x = c(final_2022, ymd("2022-12-31")))

ggsave("plots/approval/net_approval_current.png",
       device = png,
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)  
