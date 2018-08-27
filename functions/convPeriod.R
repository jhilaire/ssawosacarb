convPeriod  <- function(df) {
  
  out <- df %>% 
    mutate(period = as.numeric(format(period, "%Y")))
  
  return(out)
}