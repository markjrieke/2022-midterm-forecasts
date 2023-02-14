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
# - need n_sims in env to run

# setup ------------------------------------------------------------------------

library(riekelib)

# setup themes
extrafont::loadfonts(device = "win")
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = "Roboto Slab", 
                         base_size = 14) +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"),
                   plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown())
)

# need sim_preds in env to run
dem_uncontested <-
  elections %>%
  filter(race == "House",
         candidate_name_REP == "-") %>%
  nrow()

# set parameters for live forecast
plausible_sims <- seq(1, n_sims, 1)
live_forecast <- sim_preds %>% filter(race != "Governor")

# set plot colors
dem_blu <- MetBrewer::MetPalettes$Benedictus[[1]][12]
rep_red <- MetBrewer::MetPalettes$Benedictus[[1]][2]
split_purp <- MetBrewer::MetPalettes$Klimt[[1]][6]

# remove large/unneeded objects
rm(demographics,
   elections,
   elections_mod,
   elections_model,
   elections_predict,
   elections_train,
   gcb,
   poll_averages,
   poll_leaders,
   polling_error,
   polls_gcb,
   pvi,
   sim_data,
   sim_mod,
   sim_preds)

# functions --------------------------------------------------------------------
  
# function for updating the live forecast
# doesn't make any sim updates, just returns the current live forecast
update_live_forecast <- function(called_winner,
                                 race_new,
                                 state_new,
                                 seat_new) {
  
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
           seats_upper = qnorm(0.9, seats, seats_sd),
           called_race = race_new,
           called_state = state_new,
           called_seat = seat_new,
           called_winner = called_winner) %>%
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
    
    plausible_sims <<- seq(1, n_sims, 1)
    current_live_forecast <<- update_live_forecast("new", "new", "new", "new")
    
  } else {
  
    # pull sims that agree with this called race
    winner_sims <-
      live_forecast %>%
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
                update_live_forecast(called_winner, race_new, state_new, seat_new))
    
  }
  
  # write out updates in the event of a crash
  current_live_forecast %>%
    write_csv("models/live/current_live_forecast.csv")
  
}

# plotting function to make it all pretty-pretty
plot_current_results <- function() {
  
  # get current toplines
  last_topline <- 
    current_live_forecast %>%
    filter(timestamp == max(timestamp))
  
  p_house <- last_topline %>% filter(race == "House") %>% pull(p_dem_win)
  p_senate <- last_topline %>% filter(race == "Senate") %>% pull(p_dem_win)
  
  # get current leaders
  leader_house <- if (p_house >= 0.5) "Democrats" else "Republicans"
  leader_senate <- if (p_senate >= 0.5) "Democrats" else "Republicans"
  
  # get current leader p
  leader_house_p <- if (p_house >= 0.5) p_house else 1 - p_house
  leader_senate_p <- if (p_senate >= 0.5) p_senate else 1 - p_senate
  
  leader_house_p <- scales::label_percent()(leader_house_p)
  leader_senate_p <- scales::label_percent()(leader_senate_p)
  
  # set leader colors
  color_house <- if (leader_house == "Democrats") dem_blu else rep_red
  color_senate <- if (leader_senate == "Democrats") dem_blu else rep_red
  
  # get number of called races/remaining sims
  called_races <- nrow(current_live_forecast)/2 - 1
  remaining_sims <- scales::label_comma()(last_topline$sims[1])
  total_sims <- scales::label_comma()(n_sims)
  
  # get current time
  current_time <- scales::label_time(format = "%H:%M", tz = "America/Chicago")(last_topline$timestamp[1])
  
  # construct title
  if (leader_house == leader_senate & leader_house == "Democrats") {
    
    plot_title <-
      glue::glue("**{color_text(leader_house, color_house)}** are favored to keep the house and senate")
    
  } else if (leader_house == leader_senate) {
    
    plot_title <-
      glue::glue("**{color_text(leader_house, color_house)}** are favored to take back the house and senate")
    
  } else {
    
    plot_title <-
      glue::glue("Control of congress is expected to be **{color_text('split across chambers', split_purp)}**")
    
  }
  
  # construct caption
  plot_caption <-
    glue::glue("{called_races} called races\n{remaining_sims} used for this estimate.")
  
  # probability plot!
  prob_plot <- 
    current_live_forecast %>% 
    ggplot(aes(x = timestamp)) + 
    geom_hline(yintercept = 0.5, 
               linetype = "dashed") +
    geom_step(aes(y = 1 - p_dem_win), 
              color = rep_red,
              size = 1) + 
    geom_step(aes(y = p_dem_win), 
              color = dem_blu,
              size = 1) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_time(labels = scales::label_time(format = "%H:%M", tz = "America/Chicago")) +
    facet_wrap(~race) + 
    expand_limits(y = c(0, 1)) + 
    labs(title = plot_title,
         subtitle = glue::glue("Probability of controlling either chamber as of {current_time}"),
         x = NULL,
         y = NULL,
         caption = plot_caption)
  
  # seat plot!
  seat_plot <- 
    current_live_forecast %>%
    rename_with(~paste0("dem_", .x), ends_with("er")) %>%
    rename(dem_seats = seats) %>%
    mutate(rep_seats = if_else(race == "House", 435 - dem_seats, 100 - dem_seats),
           rep_seats_lower = qnorm(0.1, rep_seats, seats_sd),
           rep_seats_upper = qnorm(0.9, rep_seats, seats_sd)) %>%
    ggplot(aes(x = timestamp)) +
    
    # add ribbons
    geom_ribbon(aes(ymin = rep_seats_lower,
                    ymax = rep_seats_upper),
                fill = rep_red,
                alpha = 0.15) +
    geom_ribbon(aes(ymin = dem_seats_lower,
                    ymax = dem_seats_upper),
                fill = dem_blu,
                alpha = 0.15) +
    
    # add rep line
    geom_line(aes(y = rep_seats),
              color = "white",
              size = 3) +
    geom_line(aes(y = rep_seats),
              color = rep_red,
              size = 1) +
    
    # add dem line
    geom_line(aes(y = dem_seats),
              color = "white",
              size = 3) +
    geom_line(aes(y = dem_seats),
              color = dem_blu,
              size = 1) + 
    
    # tune up
    facet_wrap(~race, 
               scales = "free_y") +
    scale_x_time(labels = scales::label_time(format = "%H:%M", tz = "America/Chicago")) +
    labs(title = plot_title,
         subtitle = glue::glue("Expected number of seats controlled as of {current_time}"),
         x = NULL,
         y = NULL,
         caption = plot_caption)
  
  current_prob_plot <<- prob_plot
  current_seat_plot <<- seat_plot
  
  # print out expected prob error
  err <- 
    tibble(sims = last_topline$sims[1]) %>%
    mutate(alpha = sims/2,
           beta = sims/2) %>%
    beta_interval(alpha, beta) %>%
    mutate(err = scales::label_percent(accuracy = 0.1)(ci_upper - 0.5)) %>%
    pull(err)
  
  message(glue::glue("A naive estimate of error on {remaining_sims} sims is +/-{err}"))
  
}

new_sims <- function(mean, sd, new_sims = n_sims) {
  
  tibble(sim = seq(1, n_sims, 1),
         .pred = rnorm(n_sims, mean, sd)) %>%
    mutate(winner = if_else(.pred >= 0.5, "dem", "rep"))
  
}

# arbitrage the number of sims back up to 40,000
resimulate <- function() {
  
  # update the live forecast based on called races
  #live_forecast <<- 
  live_tmp <-
    live_forecast %>%
    nplyr::nest_filter(data, sim %in% plausible_sims) %>%
    mutate(.pred_mean = map_dbl(data, ~mean(.x$.pred)),
           .pred_sd = map_dbl(data, ~sd(.x$.pred))) %>%
    select(-data) %>%
    mutate(data = pmap(list(.pred_mean, .pred_sd), ~new_sims(..1, ..2)))
  
  # rehash the plausible sims back up to 40,000
  plausible_sims <<- seq(1, n_sims, 1)
  
}

# live results -----------------------------------------------------------------

add_called_race(new = TRUE)

message("line break!")

plot_current_results()
current_prob_plot; ggquicksave("models/live/current_prob_plot.png", height = 4.78, width = 9.33)
current_seat_plot; ggquicksave("models/live/current_seat_plot.png", height = 4.78, width = 9.33)
current_live_forecast

# closer calls / senate calls---------------------------------------------------

add_called_race("rep", "Senate", "Indiana")
add_called_race("rep", "Senate", "South Carolina")
add_called_race("rep", "Senate", "Kentucky")
add_called_race("rep", "Senate", "Oklahoma")
add_called_race("rep", "Senate", "Oklahoma", "Class II")
add_called_race("rep", "Senate", "Alabama")
add_called_race("rep", "Senate", "Florida")
add_called_race("dem", "Senate", "Vermont")
add_called_race("rep", "Senate", "Arkansas")
add_called_race("rep", "Senate", "South Dakota")
add_called_race("dem", "Senate", "New York")
add_called_race("rep", "Senate", "Kansas")

# solids -----------------------------------------------------------------------

add_called_race("dem", "House", "Delaware", "District 1")

add_called_race("rep", "House", "Florida", "District 1")
add_called_race("rep", "House", "Florida", "District 2")
add_called_race("rep", "House", "Florida", "District 3")
add_called_race("rep", "House", "Florida", "District 4")
add_called_race("rep", "House", "Florida", "District 6")
add_called_race("rep", "House", "Florida", "District 7")
add_called_race("rep", "House", "Florida", "District 8")
add_called_race("dem", "House", "Florida", "District 10")
add_called_race("rep", "House", "Florida", "District 11")
add_called_race("rep", "House", "Florida", "District 12")
add_called_race("rep", "House", "Florida", "District 15")
add_called_race("rep", "House", "Florida", "District 16")
add_called_race("rep", "House", "Florida", "District 17")
add_called_race("rep", "House", "Florida", "District 18")
add_called_race("rep", "House", "Florida", "District 19")
add_called_race("dem", "House", "Florida", "District 20")
add_called_race("rep", "House", "Florida", "District 21")
add_called_race("dem", "House", "Florida", "District 22")
add_called_race("dem", "House", "Florida", "District 24")
add_called_race("dem", "House", "Florida", "District 25")
add_called_race("rep", "House", "Florida", "District 26")
add_called_race("rep", "House", "Florida", "District 28")



add_called_race("rep", "House", "Georgia", "District 10")
add_called_race("rep", "House", "Georgia", "District 14")
add_called_race("rep", "House", "Georgia", "District 3")

add_called_race("dem", "House", "Illinois", "District 1")
