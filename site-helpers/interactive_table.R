# generate a table of individual candidate results
prep_table <- function(.data, race_input) {
  
  race_candidates <-
    .data %>%
    filter(race == race_input,
           model_date == max(model_date)) %>%
    mutate(p_dem_win = round(100*p_dem_win),
           .pred = round(100*.pred, 1)) %>%
    select(state, 
           seat,
           candidate_name_DEM,
           candidate_name_REP,
           p_dem_win,
           .pred)
  
  if (race_input %in% c("Senate", "Governor")) {
    
    race_candidates <-
      race_candidates %>%
      select(-seat)
    
  } else {
    
    race_candidates <-
      race_candidates %>%
      mutate(seat = str_remove(seat, "District "),
             state = paste(state, seat)) %>%
      select(-seat)
    
  }
  
  # # build out the table
  candidate_table <-
    browsable(
      
      # search bar at the top
      tagList(
        div(
          style = "margin-bottom: 0.75rem",
          tags$input(
            type = "text",
            placeholder = "Search for a state... or click to sort by column values",
            style = "padding: 0.25rem 0.5rem; width: 100%",
            oninput = "Reactable.setSearch('state-search', this.value)"
          )
        ),
        
        race_candidates %>%
          reactable(
            defaultPageSize = 52,
            compact = TRUE,
            highlight = TRUE,
            elementId = "state-search",
            defaultColDef = colDef(align = "center",
                                   vAlign = "center",
                                   minWidth = 100),
            columns = list(
              state = colDef(name = ""),
              candidate_name_DEM = colDef(name = "", style = paste("color:", dem_blu)),
              candidate_name_REP = colDef(name = "", style = paste("color:", rep_red)),
              p_dem_win = colDef(
                name = "Odds",
                cell = JS("function(cellInfo) {if(cellInfo.value < 50) {return 100 - cellInfo.value + ' in 100'} else {return cellInfo.value + ' in 100'}}"),
                style = function(value) {
                  list(color = if (value >= 50) dem_blu else rep_red)
                }
              ),
              .pred = colDef(
                name = "Two-party margin",
                cell = JS("function(cellInfo) {if(cellInfo.value < 50) {return 'R +' + Math.round(20 * (50 - cellInfo.value))/10} else {return 'D +' + Math.round(20 * (cellInfo.value - 50))/10}}"),
                style = function(value) {
                  list(color = if (value >= 50) dem_blu else rep_red)
                }
              )
            )
          )
        
      )
    )
  
  return(candidate_table)
  
}