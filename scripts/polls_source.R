# libraries ----
library(tidyverse)

# read in data ----

# fte
pres_pollaverages_1968_2016 <- read_csv("https://github.com/fivethirtyeight/data/raw/master/polls/pres_pollaverages_1968-2016.csv")
pres_pollaverages_2020 <- read_csv("https://projects.fivethirtyeight.com/2020-primary-data/pres_primary_avgs_2020.csv")
pres_primary_avgs_1980_2016 <- read_csv("https://github.com/fivethirtyeight/data/raw/master/polls/pres_primary_avgs_1980-2016.csv")
pres_primary_avgs_2020 <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_poll_averages_2020.csv")
pres_primary_poll_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_primary_polls_historical.csv")
pres_general_poll_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls_historical.csv")
senate_polls_2018_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls_historical.csv")
house_polls_2018_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls_historical.csv")
governor_polls_2018_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls_historical.csv")
pres_approval_poll_2016_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_approval_polls_historical.csv")
generic_polls_2018_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls_historical.csv")

# econ
all_polls_2008 <- read_csv("https://github.com/TheEconomist/us-potus-model/raw/master/data/all_polls_2008.csv")
all_polls_2012 <- read_csv("https://github.com/TheEconomist/us-potus-model/raw/master/data/all_polls_2012.csv")
all_polls_2016 <- read_csv("https://github.com/TheEconomist/us-potus-model/raw/master/data/all_polls.csv")

# upshot
polls_2012 <- read_csv("https://github.com/TheUpshot/2004-2012-presidential-election-model/raw/master/data/2012/polls.csv")
polls_2004 <- read_csv("https://github.com/TheUpshot/2004-2012-presidential-election-model/raw/master/data/2004/polls.csv")
senate_polls_historical <- read_csv("https://github.com/TheUpshot/leo-senate-model/raw/master/model/data/polls/historic-polls/senate_polls.csv")

# write data ----

# fte
path <- "data/polls/src/fte/"
write_csv(pres_pollaverages_1968_2016, paste0(path, "pres_pollaverages_1968_2016.csv"))
write_csv(pres_pollaverages_2020, paste0(path, "pres_pollaverages_2020.csv"))
write_csv(pres_primary_avgs_1980_2016, paste0(path, "pres_primary_avgs_1980_2016.csv"))
write_csv(pres_primary_avgs_2020, paste0(path, "pres_primary_avgs_2020.csv"))
write_csv(pres_primary_poll_2020, paste0(path, "pres_primary_poll_2020.csv"))
write_csv(pres_general_poll_2020, paste0(path, "pres_general_poll_2020.csv"))
write_csv(senate_polls_2018_2020, paste0(path, "senate_polls_2018_2020.csv"))
write_csv(house_polls_2018_2020, paste0(path, "house_polls_2018_2020.csv"))
write_csv(governor_polls_2018_2020, paste0(path, "governor_polls_2018_2020.csv"))
write_csv(pres_approval_poll_2016_2020, paste0(path, "pres_approval_poll_2016_2020.csv"))
write_csv(generic_polls_2018_2020, paste0(path, "generic_polls_2018_2020.csv"))

# econ
path <- "data/polls/src/econ/"
write_csv(all_polls_2008, paste0(path, "all_polls_2008.csv"))
write_csv(all_polls_2012, paste0(path, "all_polls_2012.csv"))
write_csv(all_polls_2016, paste0(path, "all_polls_2016.csv"))

# upshot
path <- "data/polls/src/upshot/"
write_csv(polls_2012, paste0(path, "polls_2012.csv"))
write_csv(polls_2004, paste0(path, "polls_2004.csv"))
write_csv(senate_polls_historical, paste0(path, "senate_polls_historical.csv"))

# clean up environment ----
rm(list = ls())
