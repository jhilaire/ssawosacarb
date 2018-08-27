getSSPvalue <- function(i_year, i_data, i_var, i_scenario = "SSP2-Ref-SPA0-V15", i_region="AFR") {
  
  if (i_year %in% unique(i_data$period)) {
    out = i_data %>% 
      filter(period == i_year, 
             region == i_region, 
             scenario == i_scenario, 
             variable == i_var) %>% 
      select(value) %>% 
      as.numeric()
  } else {
    
    year_low  = unique(i_data$period)[which(unique(i_data$period) > i_year)[1] -1]
    year_high = unique(i_data$period)[which(unique(i_data$period) > i_year)[1]]
    
    val_low = i_data %>% 
      filter(period == year_low, 
             region == i_region, 
             scenario == i_scenario, 
             variable == i_var) %>% 
      select(value) %>% 
      as.numeric()
    val_high = i_data %>% 
      filter(period == year_high, 
             region == i_region, 
             scenario == i_scenario, 
             variable == i_var) %>% 
      select(value) %>% 
      as.numeric()
    
    out = approx(c(year_low, year_high), c(val_low, val_high), i_year)$y
  }
  
  return(out)
}