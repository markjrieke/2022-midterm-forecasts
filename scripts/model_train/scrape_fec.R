# libraries ----
library(tidyverse)

# read in / save data ----

# function for read/save
save_fec <- function(year) {
  
  short_name <- str_remove(as.character(year), "20")
  
  read_delim(paste0("data/fec/weball", short_name, ".txt"),
             "|",
             col_names = FALSE) %>%
    select(candidate = X2,
           party = X5,
           receipts = X6,
           disbursements = X8,
           ind_contributors = X18) %>%
    arrange(candidate) %>%
    write_csv(paste0("data/fec/fec_src_",
                     year,
                     ".csv"))
  
}

# pull/save
save_fec(2018)
save_fec(2020)

# pull into one common frame
bind_rows(read_csv("data/fec/fec_src_2018.csv"),
          read_csv("data/fec/fec_src_2020.csv")) %>%
  distinct(candidate) %>%
  arrange(candidate) %>%
  write_csv("data/fec/fec_names.csv")


# must change 2020 CA-4 DELGADO, ANTONIO to DELGADO, C. ANTONIO prior to left_join()

