# prep an interactive histogram to be passed to ggiraph
prep_histogram <- function(.data, 
                           max_seats, 
                           plot_title, 
                           plot_subtitle,
                           dem_color = dem_blu,
                           rep_color = rep_red,
                           split_color = purple) {
  
  col_border <- if(max_seats == 100) site_color else NA
  
  .data %>%
    rename(dem_seats = n) %>%
    mutate(rep_seats = max_seats - dem_seats,
           control = case_when(dem_seats > max_seats/2 ~ dem_color,
                               dem_seats < max_seats/2 ~ rep_color,
                               TRUE ~ split_color)) %>%
    count(dem_seats, rep_seats, control) %>%
    rename(pct = n) %>%
    arrange(desc(dem_seats)) %>%
    mutate(pct = pct/10000,
           pct_raw = pct,
           pct_dem = cumsum(pct_raw)) %>%
    arrange(desc(rep_seats)) %>%
    mutate(pct_rep = cumsum(pct_raw),
           across(c(pct, pct_dem, pct_rep), ~round(.x * 100)),
           pct_text = case_when(control == dem_color & pct_dem < 1 ~ glue::glue("Democrats have a < 1% chance \nof controlling at least {dem_seats} seats"),
                                control == dem_color ~ glue::glue("Democrats have a {pct_dem}% chance\nof controlling at least {dem_seats} seats"),
                                control == split_color ~ glue::glue("Democrats have a {pct_dem}% chance \nof controlling at least 50 seats \nand a {pct}% chance of controlling\nexactly 50 seats"),
                                control == rep_color & pct_rep < 1 ~ glue::glue("Republicans have a < 1% chance \nof controlling at least {rep_seats} seats"),
                                control == rep_color ~ glue::glue("Republicans have a {pct_rep}% chance \nof controlling at least {rep_seats} seats"))) %>%
    ggplot(aes(x = rep_seats,
               y = pct_raw,
               fill = control,
               tooltip = pct_text)) +
    geom_col_interactive(alpha = 0.75,
                         width = 1,
                         color = col_border) +
    scale_fill_identity() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.y = element_blank()) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         caption = glue::glue("Last updated on {last_run}\nData courtesy of FiveThirtyEight"))
  
}

# function for generating house/senate probability plots
prep_probability <- function(.data, race, nudge_days = 6) {
  
  .data %>%
    
    # add in color interpolation for tooltip text and select relevant cols
    mutate(color_ramp = map_chr(p_dem_win, interpolate_fill)) %>%
    select(model_date,
           color_ramp,
           dem = p_dem_win) %>%
    
    # format values for tooltip
    mutate(rep = 1 - dem,
           date_format = format_date(model_date),
           dem_format = paste0(round(dem*100), "%"),
           rep_format = paste0(round(rep*100), "%"),
           tooltip_text = glue::glue("{date_format}:\nDem: {dem_format}\nRep: {rep_format}")) %>%
    
    # format values for dot displaying current day's probability
    mutate(dem_final = if_else(model_date == max(model_date), dem, as.numeric(NA)),
           rep_final = if_else(model_date == max(model_date), rep, as.numeric(NA)),
           dem_final_format = if_else(model_date == max(model_date), dem_format, NA_character_),
           rep_final_format = if_else(model_date == max(model_date), rep_format, NA_character_)) %>%
    
    # plot !
    ggplot(aes(x = model_date)) +
    
    # rep trendline
    geom_line(aes(y = rep),
              size = 3.25,
              color = site_color) +
    geom_line(aes(y = rep),
              size = 1.25,
              color = rep_red,
              alpha = 0.75) +
    
    # rep current day
    geom_point(aes(y = rep_final),
               size = 3,
               alpha = 1,
               color = rep_red,
               fill = NA,
               na.rm = TRUE) +
    geom_text(aes(y = rep_final,
                  label = rep_final_format),
              nudge_x = nudge_days,
              color = rep_red,
              family = "Roboto Slab",
              fontface = "bold",
              alpha = 0.75,
              na.rm = TRUE) +
    
    # dem trendline
    geom_line(aes(y = dem),
              size = 3.25,
              color = site_color) +
    geom_line(aes(y = dem),
              size = 1.25,
              color = dem_blu,
              alpha = 0.75) +
    
    # dem current day
    geom_point(aes(y = dem_final),
               size = 3,
               alpha = 1,
               color = dem_blu,
               fill = NA,
               na.rm = TRUE) +
    geom_text(aes(y = dem_final,
                  label = dem_final_format),
              nudge_x = nudge_days,
              color = dem_blu,
              family = "Roboto Slab",
              fontface = "bold",
              alpha = 0.75,
              na.rm = TRUE) +
    
    # tooltip
    geom_tile_interactive(aes(y = 0.5,
                              tooltip = tooltip_text,
                              fill = color_ramp),
                          width = 1,
                          height = 1,
                          size = 3,
                          alpha = 0.005) +
    scale_fill_identity() +
    
    # final labels and scales
    expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")),
                  y = c(0, 1)) +
    theme(plot.subtitle = ggtext::element_markdown(size = 10, hjust = 0)) +
    labs(subtitle = glue::glue("**Chances of controlling {race}**", .sep = ""),
         x = NULL,
         y = NULL) +
    scale_y_continuous(labels = scales::label_percent())
  
}

# function for prepping the number of seats as time goes on
prep_seats <- function(.data, race, nudge_days = 6) {
  
  if (race == "the Senate") {
    
    max_seats <- 100
    custom_expand <- 
      expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")),
                    y = c(43, 57)) 
    tooltip_center <- 50
    tooltip_height <- 14
    
  } else {
    
    max_seats <- 435
    custom_expand <-
      expand_limits(x = c(lubridate::mdy("7/1/22"), lubridate::mdy("11/8/22")),
                    y = c(135, 300))
    
    tooltip_center <- 217.5
    tooltip_height <- 165
    
  }
  
  .data %>%
    
    # add in color for tooltip & select/rename relevant cols
    mutate(color_ramp = map_chr(p_dem_win, interpolate_fill)) %>%
    select(model_date, 
           color_ramp,
           starts_with("seat")) %>%
    rename_with(.cols = starts_with("seat"),
                .fn = ~paste0(.x, "_dem")) %>%
    
    # get rep seats/interval
    mutate(seats_rep = max_seats - seats_dem,
           seats_lower_rep = max_seats - seats_upper_dem,
           seats_upper_rep = max_seats - seats_lower_dem) %>%
    
    # formatting for tooltip
    mutate(date_format = format_date(model_date),
           dem_format = round(seats_dem, digits = 1),
           rep_format = round(seats_rep, digits = 1),
           tooltip_text = glue::glue("{date_format}:\nDem: {dem_format}\nRep: {rep_format}")) %>%
    
    # formatting for current day display
    mutate(dem_final = if_else(model_date == max(model_date), seats_dem, as.numeric(NA)),
           rep_final = if_else(model_date == max(model_date), seats_rep, as.numeric(NA)),
           dem_final_format = if_else(model_date == max(model_date), dem_format, as.numeric(NA)),
           rep_final_format = if_else(model_date == max(model_date), rep_format, as.numeric(NA))) %>%
    
    # plot !
    ggplot(aes(x = model_date)) +
    
    # ribbons for both parties
    geom_ribbon(aes(ymin = seats_lower_rep,
                    ymax = seats_upper_rep),
                fill = rep_red,
                alpha = 0.25) +
    geom_ribbon(aes(ymin = seats_lower_dem,
                    ymax = seats_upper_dem),
                fill = dem_blu,
                alpha = 0.25) +
    
    # rep trendline
    geom_line(aes(y = seats_rep),
              size = 3.25,
              color = site_color) +
    geom_line(aes(y = seats_rep),
              size = 1.25,
              color = rep_red,
              alpha = 0.75) +
    
    # rep current day
    geom_point(aes(y = rep_final),
               size = 3,
               alpha = 1,
               color = rep_red,
               fill = NA,
               na.rm = TRUE) +
    geom_text(aes(y = rep_final,
                  label = rep_final_format),
              nudge_x = nudge_days,
              color = rep_red,
              family = "Roboto Slab",
              fontface = "bold",
              alpha = 0.75,
              na.rm = TRUE) +
    
    # dem trendline
    geom_line(aes(y = seats_dem),
              size = 3.25,
              color = site_color) +
    geom_line(aes(y = seats_dem),
              size = 1.25,
              color = dem_blu,
              alpha = 0.75) +
    
    # dem current day
    geom_point(aes(y = dem_final),
               size = 3, 
               alpha = 1,
               color = dem_blu,
               fill = NA,
               na.rm = TRUE) +
    geom_text(aes(y = dem_final,
                  label = dem_final_format),
              nudge_x = nudge_days,
              color = dem_blu,
              family = "Roboto Slab",
              fontface = "bold",
              alpha = 0.75,
              na.rm = TRUE) +
    geom_tile_interactive(aes(y = tooltip_center,
                              tooltip = tooltip_text,
                              fill = color_ramp),
                          width = 1,
                          height = tooltip_height,
                          size = 3,
                          alpha = 0.005) +
    scale_fill_identity() +
    
    # final scales and formatting
    custom_expand + 
    theme(plot.subtitle = ggtext::element_markdown(size = 10, hjust = 0)) +
    labs(subtitle = "**Average seats controlled by party**",
         x = NULL,
         y = NULL) +
    scale_y_continuous(labels = scales::label_number(1))
  
}