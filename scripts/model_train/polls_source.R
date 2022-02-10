# libraries ----
library(tidyverse)

# fte ----
path <- "data/polls/src/fte/"

read_csv("https://github.com/fivethirtyeight/data/raw/master/polls/pres_pollaverages_1968-2016.csv") %>%
  write_csv(paste0(path, "pres_pollaverages_1968-2016.csv"))
  
read_csv("https://projects.fivethirtyeight.com/2020-primary-data/pres_primary_avgs_2020.csv") %>%
  write_csv(paste0(path, "pres_primary_avgs_2020.csv"))

read_csv("https://github.com/fivethirtyeight/data/raw/master/polls/pres_primary_avgs_1980-2016.csv") %>%
  write_csv(paste0(path, "pres_primary_avgs_1980-2016.csv"))

read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_poll_averages_2020.csv") %>%
  write_csv(paste0(path, "presidential_poll_averages_2020.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_primary_polls_historical.csv") %>%
  write_csv(paste0(path, "president_primary_polls_historical.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls_historical.csv") %>%
  write_csv(paste0(path, "president_polls_historical.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls_historical.csv") %>%
  write_csv(paste0(path, "senate_polls_historical.csv"))

read_csv("https://github.com/fivethirtyeight/data/raw/master/august-senate-polls/august_senate_polls.csv") %>%
  write_csv(paste0(path, "august_senate_polls.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls_historical.csv") %>%
  write_csv(paste0(path, "house_polls_historical.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls_historical.csv") %>%
  write_csv(paste0(path, "governor_polls_historical.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_approval_polls_historical.csv") %>%
  write_csv(paste0(path, "president_approval_polls_historical.csv"))

read_csv("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv") %>%
  write_csv(paste0(path, "approval_topline.csv"))

read_csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls_historical.csv") %>%
  write_csv(paste0(path, "generic_ballot_polls_historical.csv"))

read_csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_polllist.csv") %>%
  write_csv(paste0(path, "generic_polllist.csv"))

read_csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_ballot.csv") %>%
  write_csv(paste0(path, "generic_ballot.csv"))

read_csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_polllist.csv") %>%
  write_csv(paste0(path, "generic_polllist_2018.csv"))

read_csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_ballot.csv") %>%
  write_csv(paste0(path, "generic_ballot_2018.csv"))

# econ ----
path <- "data/polls/src/econ/"

read_csv("https://github.com/TheEconomist/us-potus-model/raw/master/data/all_polls_2008.csv") %>%
  write_csv(paste0(path, "all_polls_2008.csv"))

read_csv("https://github.com/TheEconomist/us-potus-model/raw/master/data/all_polls_2012.csv") %>%
  write_csv(paste0(path, "all_polls_2012.csv"))

read_csv("https://github.com/TheEconomist/us-potus-model/raw/master/data/all_polls.csv") %>%
  write_csv(paste0(path, "all_polls.csv"))

# upshot ----
path <- "data/polls/src/upshot/"

read_csv("https://github.com/TheUpshot/2004-2012-presidential-election-model/raw/master/data/2012/polls.csv") %>%
  write_csv(paste0(path, "polls_2012.csv"))

read_csv("https://github.com/TheUpshot/2004-2012-presidential-election-model/raw/master/data/2004/polls.csv") %>%
  write_csv(paste0(path, "polls_2004.csv"))

read_csv("https://github.com/TheUpshot/leo-senate-model/raw/master/model/data/polls/historic-polls/senate_polls.csv") %>%
  write_csv(paste0(path, "senate_polls.csv"))

# clean up environment ----
rm(list = ls())

