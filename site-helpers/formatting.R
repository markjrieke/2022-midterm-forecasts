# ---------------------------formatting-functions-------------------------------

# format date for display (e.g. 2022-8-3 -> Aug 3rd)
format_date <- function(date) {
  
  month_day <- lubridate::day(date)
  day_append <- case_when(month_day %in% c(1, 21, 31) ~ "st",
                          month_day %in% c(2, 22) ~ "nd",
                          month_day %in% c(3, 23) ~ "rd",
                          TRUE ~ "th")
  
  date_formatted <- paste0(lubridate::month(date, label = TRUE),
                           " ", month_day, day_append)
  
  return(date_formatted)
  
}

# format title 
# cutoffs based on economist 2020 house cutoffs
format_title <- function(p, race, dem = "Democrats", rep = "Republicans") {
  
  plural <- if(dem == "Democrats") "are" else "is"
  
  title <- 
    case_when(p > 0.35 & p < 0.65 ~ paste("***It's a toss-up*** in ", race),
              p > 0.50 & p < 0.85 ~ paste0("**", color_text(dem, dem_blu), "** ", plural, " **likely** to win ", race),
              p > 0.50            ~ paste0("**", color_text(dem, dem_blu), "** ", plural, " **very likely** to win ", race),
              p < 0.50 & p > 0.15 ~ paste0("**", color_text(rep, rep_red), "** ", plural, " **likely** to win ", race),
              p < 0.50            ~ paste0("**", color_text(rep, rep_red), "** ", plural, " **very likely** to win ", race))
  
  return(title)
  
}

# format subtitle
format_subtitle <- function(p, race, dem = "Democrats", rep = "Republicans") {
  
  plural <- if(dem == "Democrats") "win" else "win"
  winner <- if(p >= 0.5) dem else rep
  winner_color <- if(winner == dem) dem_blu else rep_red
  
  prob <- if(p >= 0.5) p else 1 - p
  prob <- paste0(round(prob * 100), "%")
  
  subtitle <-
    paste0("Of the model's 10,000 simulations of ",
           race,
           ", <br>**",
           color_text(winner, winner_color),
           "** ",
           plural,
           " about **",
           prob,
           "** of the time")
  
  return(subtitle)
  
}

# function for returning a color between rep_red/dem_blu based on a value between 0/1
interpolate_fill <- function(x) {
  
  color_rgb <- colorRamp(c(rep_red, dem_blu))
  rgb_vals <- color_rgb(x)
  hex <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
  
  return(hex)
  
}
