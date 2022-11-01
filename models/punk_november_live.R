# notes ------------------------------------------------------------------------

# this script is definitely not in good form, just something I'm using on 
# election night to update the win probability of each chamber as races are 
# called.
#
# if it gets to too few plausible sims remaining, I'll have to cannibalize the
# live forecast. FTE does some fancy re-regression, but I don't have the time or
# wearewithal to do so on my end lol.
#
# tl/dr: don't expect this to just run later without issues

# some reqs:
# - run through punk_november_predict
# - need sim_preds in env to run

# setup ------------------------------------------------------------------------

# need sim_preds in env to run
dem_uncontested <-
  elections %>%
  filter(race == "House",
         candidate_name_REP == "-") %>%
  nrow()

# set parameters for live forecast
plausible_sims <- seq(1, 10000, 1)
live_forecast <- sim_preds %>% filter(race != "Governor")

# set plot colors
dem_blu <- MetBrewer::MetPalettes$Benedictus[[1]][12]
rep_red <- MetBrewer::MetPalettes$Benedictus[[1]][2]

# functions --------------------------------------------------------------------
  
# function for updating the live forecast
# doesn't make any sim updates, just returns the current live forecast
update_live_forecast <- function() {
  
  live_forecast %>%
    unnest(data) %>%
    filter(sim %in% plausible_sims) %>%
    count(race, sim, winner) %>%
    filter(winner == "dem") %>%
    select(-winner) %>%
    mutate(n = case_when(race == "House" ~ n + as.numeric(dem_uncontested),
                         TRUE ~ n + 36),
           winner = case_when(race == "House" & n >= 435/2 ~ "dem",
                              race == "Senate" & n >= 50 ~ "dem",
                              TRUE ~ "rep")) %>%
    nest(data = -race) %>%
    mutate(timestamp = Sys.time(),
           sims = map_int(data, nrow),
           p_dem_win = case_when(race == "House" ~ map_dbl(data, ~sum(.x$n > 435/2)),
                                 race == "Senate" ~ map_dbl(data, ~sum(.x$n >= 50))),
           p_dem_win = p_dem_win/sims,
           seats = map_dbl(data, ~mean(.x$n)),
           seats_sd = map_dbl(data, ~sd(.x$n)),
           seats_lower = qnorm(0.1, seats, seats_sd),
           seats_upper = qnorm(0.9, seats, seats_sd)) %>%
    select(-data)
  
}

# update the live forecast based on a new race call
# wraps update_live_forecast() --- appends current_live_forecast
add_called_race <- function(called_winner,
                            race_new = NULL,
                            state_new = NULL,
                            seat_new = "Class III", # only 1 Class II race (OK-s)
                            new = FALSE) {
  
  if (new) {
    
    plausible_sims <<- seq(1, 10000, 1)
    current_live_forecast <<- update_live_forecast()
    
  } else {
  
    # pull sims that agree with this called race
    winner_sims <-
      sim_preds %>%
      filter(race == race_new,
             state == state_new,
             seat == seat_new) %>%
      unnest(data) %>%
      filter(winner == called_winner) %>%
      pull(sim)
    
    # update plausible sims
    plausible_sims <<- plausible_sims[plausible_sims %in% winner_sims]
    
    # append with new set of plausible sims
    current_live_forecast <<- 
      bind_rows(current_live_forecast,
                update_live_forecast())
    
  }
  
}

# plotting function to make it all pretty-pretty
plot_current_results <- function() {
  
  # get current toplines
  last_topline <- 
    current_live_forecast %>%
    filter(timestamp == max(timestamp))
  
  p_house <- last_topline %>% filter(race == "House") %>% pull(p_dem_win)
  p_senate <- last_topline %>% filter(race == "Senate") %>% pull(p_dem_win)
  
  # get number of called races
  called_races <- nrow(current_live_forecast)/2 - 1
  
  # construct title
  
  
}

# live results -----------------------------------------------------------------

add_called_race(new = TRUE)
add_called_race("dem", "Senate", "Pennsylvania")
add_called_race("dem", "Senate", "Georgia")
add_called_race("dem", "Senate", "Oregon")
add_called_race("rep", "Senate", "Oklahoma", "Class III")



