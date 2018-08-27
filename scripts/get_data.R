if (u_getandpprocdata) {
  #== GET DATA ============================
  # Get SSA countries
  data_ssa <- read_xlsx("data/SSA_countries.xlsx") %>% 
    select(Country, ISO, COUNTRY_PLATTS, COUNTRY_COALSWARM) %>% 
    filter(ISO != "ZAF") # remove South Africa
  
  # Power plant data (Boom and Bust 2017, PLATTS 2017)
  data_pp <- list()
  data_pp$coal  = readxl::read_xlsx("data/Global Coal Plant Tracker Feb 2017c.xlsx", sheet = "Projects") %>%
    filter(Region == "Africa and Middle East") %>%
    filter(!Country %in% c("Egypt", "Iran", "Israel", "Jordan", "Morocco", "Oman", "South Africa", "Syria", "United Arab Emirates"))
  
  if (!file.exists("data/PLATTS 2017/ALLUNITS_AFRICA.RData")) {
    data_pp_other <- read.csv("data/PLATTS 2017/ALLUNITS.csv")
    data_abbrev   <- read_xlsx("data/PLATTS 2017/ABBREV.xlsx")
    
    data_pp_other <- data_pp_other %>% 
      filter(AREA == "AFRICA") %>% 
      filter(COUNTRY %in% data_ssa$COUNTRY_PLATTS) %>% 
      left_join(data_abbrev, 
                by=c("FUEL"="ABBREV"))
    
    save(data_pp_other, file = "data/PLATTS 2017/ALLUNITS_AFRICA.RData")
  } else {
    load("data/PLATTS 2017/ALLUNITS_AFRICA.RData")
  }
  

  tmp_data_pp_other <- data_pp_other %>% 
    mutate(STATUS = toupper(STATUS)) %>% 
    filter(!(STATUS %in% "OPR" & YEAR <= 2015)) %>% 
    filter(FUELCATEGORY != "UNK") %>% 
    group_by(COUNTRY,FUELCATEGORY,STATUS) %>% 
    summarize(CAPACITY=sum(MW)) %>% 
    ungroup() %>% 
    filter(COUNTRY %in% toupper(unique(data_pp$coal$Country))) %>%
    filter(STATUS %in% c("OPR", "CON", "PLN", "DEF", "DEL", "UNK")) %>% 
    mutate(STATUS   = factor(STATUS, levels=rev(c("OPR", "CON", "PLN", "DEF", "DEL")), ordered=TRUE)) %>% 
    mutate(FUELCATEGORY = factor(FUELCATEGORY, 
                                 levels=rev(c("COAL", "OIL", "GAS", "BIOMASS", "UR", "HYDRO", "REN", "UNK")),
                                 ordered=TRUE)) %>% 
    mutate(CAPACITY = CAPACITY/1e3) %>% 
    arrange(FUELCATEGORY, STATUS) %>% 
    group_by(FUELCATEGORY, STATUS) %>% 
    summarise(CAPACITY=sum(CAPACITY)) %>% 
    ungroup() %>% 
    spread(STATUS, CAPACITY) %>% 
    rename(Operating=OPR) %>% 
    rename(Construction=CON) %>% 
    rename(Planned=PLN) %>% 
    mutate(Shelved=DEL+DEF) %>%
    select(-DEL,-DEF) %>% 
    gather(STATUS, CAPACITY, -FUELCATEGORY) %>% 
    filter(!is.na(STATUS), STATUS != "<NA>") %>% 
    mutate(STATUS=factor(STATUS, levels=c("Operating", "Construction", "Planned", "Shelved"),
                         labels=c("Operating", "Construction", "Planned", "Shelved\n(Delayed & Deferred)"), ordered=TRUE))
  
  #== PRE-PROCESS DATA ============================
  # Select capacities under construction
  # Harmonise SSA country names
  tmp_ssa = data_ssa %>% 
    mutate(Country = substr(Country, 2, nchar(Country))) %>% 
    mutate(Country = ifelse(Country == "Democratic Republic of the Congo", "Democratic Republic of Congo", Country)) %>% 
    rename(country=Country, iso=ISO)
  
  # Remove all operational power plants before 2015 (included)
  data_pp_other2 <- data_pp_other %>% 
    mutate(STATUS = toupper(STATUS)) %>% 
    filter(!(STATUS %in% "OPR" & YEAR <= 2015))
  
  data_pp$oil <- data_pp_other2 %>% 
    filter(FUELCATEGORY == "OIL")
  data_pp$gas <- data_pp_other2 %>% 
    filter(FUELCATEGORY == "GAS")
  data_pp$bio <- data_pp_other2 %>% 
    filter(FUELCATEGORY == "BIOMASS")
  data_pp$hydro <- data_pp_other2 %>% 
    filter(FUELCATEGORY == "HYDRO")
  data_pp$ren <- data_pp_other2 %>% 
    filter(FUELCATEGORY == "REN")
  data_pp$nuc <- data_pp_other2 %>% 
    filter(FUELCATEGORY == "UR")
  data_pp$geo <- data_pp_other2 %>% 
    filter(FUEL == "GEO")
  
  load("data/IEA 2017/data_iea2017_ssa.RData")
  
  p_data_IEA_SSA <- data_iea2017_ssa %>% 
    select(-`PE Coal - Pre-permit development`, -`Coal - Pre-permit development`, -`CO2 Coal - Pre-permit development`) %>% 
    rename(
      `CO2 Coal - Pre-Construction`=`CO2 Coal - Announced`,
      `CO2 Coal - Construction`=`CO2 Coal - Permitted`,
      `Coal - Pre-Construction`=`Coal - Announced`,
      `Coal - Construction`=`Coal - Permitted`,
      `PE Coal - Pre-Construction`=`PE Coal - Announced`,
      `PE Coal - Construction`=`PE Coal - Permitted`
    )
  names(p_data_IEA_SSA) <- c(
    "year",
    "CO2 Coal and Coal Products", "CO2 Coal - Pre-Construction", "CO2 Coal - Construction", "CO2 Coal - Operating", "CO2 Coal - Shelved",
    "CO2 Oil ","CO2 Natural Gas ","CO2 Other ","CO2 Sectoral Approach (Mt of CO2)",
    "Coal and Coal Products", "Coal - Pre-Construction", "Coal - Construction", "Coal - Operating", "Coal - Shelved",
    "Oil ","Natural Gas ","Other ","CO2 Reference Approach (Mt of CO2)",
    "Total Primary Energy Supply (PJ)","Total Primary Energy Supply (Mtoe)","GDP (billion 2000 US$ using exchange rates)","GDP (billion 2000 US$ using PPPs)",
    "Population (millions)",
    "PE Coal and Coal Products", "PE Coal - Pre-Construction", "PE Coal - Construction", "PE Coal - Operating", "PE Coal - Shelved",
    "PE Peat","PE Crude, NGL and Feedstocks","PE Petroleum Products","PE Natural Gas",
    "PE Nuclear","PE Hydro","PE Geothermal","PE Solar/Wind/Other","PE Combustible Renewables and Waste",
    "PE Heat Production from non-specified comb.fuels","PE Electricity","Heat","Total Primary Energy Supply (ktoe)")  
  
} 