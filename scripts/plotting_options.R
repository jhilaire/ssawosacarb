
# Plot options for power plant capacities (scripts/plot_powerplant_capacities.R)
red_shades      <- colorRampPalette(5,colors=c("#7E3517", "#E2A76F"))
v_alphas        <- seq(0.05,1.0,(1-0.05)/5)
names(v_alphas) <- rev(c("OPR", "CON", "PLN", "DEF", "DEL", "UNK"))
v_fills_ppc         <- c(
  #"#800080", # UNK
  "#ff6600", # REN
  "#000080", # HYDRO
  "#ff0000", # UR
  "#00ff00", # BIOMASS
  "#0000ff", # GAS
  "#000000", # OIL
  red_shades(4)[1]) # COAL
names(v_fills_ppc) <- rev(c("COAL", "OIL", "GAS", "BIOMASS", "UR", "HYDRO", "REN")) 