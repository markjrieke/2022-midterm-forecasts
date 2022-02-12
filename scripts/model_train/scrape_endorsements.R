# libraries ----
library(tidyverse)
library(rvest)

# read in links ----
race_links <- 
  read_csv("data/models/midterm_model/race_links.csv") %>%
  select(-special)

# function for scraping wikipedia ----
get_endorsements <- function(cycle, race, state, link) {
  
  candidates <- 
    read_html(link) %>%
    html_elements(".endorsements-box-title") %>%
    html_text()
  
  endorsements <- 
    read_html(link) %>%
    html_elements(".endorsements-box-list") %>%
    html_text2() %>%
    as.character()
  
  endorsements_summary <- 
    tibble(cycle = cycle, 
           race = race,
           state,
           candidates = candidates,
           endorsements = endorsements)
  
  if (endorsements_summary %>% nrow() == 0) {
    
    message(paste("No endorsements returned for",
                  cycle,
                  race,
                  state))
    
  }
  
  return(endorsements_summary)
  
}

# map links over function
endorsements <- 
  race_links %>%
  as.list() %>%
  pmap_dfr(~get_endorsements(..1, ..2, ..3, ..4))

# save file to clean later
endorsements %>%
  write_csv("data/endorsements/endorsements_scrape.csv")





  
  
  
  
  
  
  
