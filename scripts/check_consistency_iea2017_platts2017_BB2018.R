# Check consistency between IEA2017 and PLATTS 2017
#==== User section ============================================================
u_years <- 2010:2015

#==== INITIALISE ==============================================================
# Own functions
get_annualcap <- function(i_df, i_years) {
  out <- data.frame(
    year = i_years,
    cap  = NA
  )
  
  for (k_year in i_years) {
    tmp_data_OPR <- i_df %>% 
      filter(STATUS == "OPR", YEAR <= k_year)
    tmp_data_RET <- i_df %>% 
      filter(STATUS == "RET", YEAR <= k_year & RETIRE > k_year)
    out$cap[which(out$year == k_year)] <- sum(tmp_data_OPR$MW) + sum(tmp_data_RET$MW)
  }
  
  return(out)
  
}


#==== GET DATA ================================================================
# Get country mapping IEA2017 and PLATTS 2017
data_ssa <- readxl::read_xlsx("data/SSA_countries.xlsx") %>% 
  select(Country, ISO, COUNTRY_PLATTS, COUNTRY_COALSWARM, COUNTRY_IEA) %>% 
  filter(ISO != "ZAF") # remove South Africa

# Get IEA World Balances 2017
if (!file.exists("data/IEA 2017/wbal_all_ktoe.RData")) {
  data_wbal <- read.csv2("data/IEA 2017/wbal_all_ktoe.csv", stringsAsFactors = FALSE)
  
  nrows <- nrow(data_wbal)
  ncols <- ncol(data_wbal)
  
  products_names <- c("Coal and coal products", "Peat and peat products", "Oil shale and oil sands", "Crude, NGL and feedstocks",
                      "Oil products", "Natural gas", "Nuclear", "Hydro", "Geothermal", "Solar/wind/other", "Biofuels and waste",
                      "Heat production from non-specified combustible fuels", "Electricity", "Heat", "Total", 
                      "Memo: Renewables", "Memo: Coal, peat and oil shale", "Memo: Primary and secondary oil", 
                      "Memo: Geothermal, solar/wind/other, heat, electricity")
  flows_names <- trimws(unique(paste(data_wbal[1, 3:ncols])))
  
  products_flows_names <- unlist(lapply(products_names, function(x) {paste0(x, "#", flows_names)}))
  
  tmp_data <- data_wbal[4:nrows, 1:ncols]
  names(tmp_data) <- c("country", "year", products_flows_names)
  for(kcol in 2:ncols) {
    tmp_data[,kcol] <- as.numeric(tmp_data[,kcol])
  }
  
  data_wbal <- tmp_data
  data_wbal_ssa <- data_wbal %>% 
    filter(country %in% u_countries)
  
  save(data_wbal, file = "data/IEA 2017/wbal_all_ktoe.RData")
  save(data_wbal_ssa, file = "data/IEA 2017/wbal_all_ktoe_SSA.RData")  
} else {
  load("data/IEA 2017/wbal_all_ktoe.RData")  
  load("data/IEA 2017/wbal_all_ktoe_SSA.RData")
}

# Get PLATTS 2017 data
load("data/PLATTS 2017/ALLUNITS_AFRICA.RData")
data_platts <- read.csv("data/PLATTS 2017/ALLUNITS.csv")


#==== PROCESS DATA ============================================================
#-- Compute load factors for SSA countries (if possible) ----------------------
lf_data <- list()
for (k_country in u_countries) {
  tmp_data <- data_wbal_ssa %>% 
    filter(country == k_country, year %in% 1980:2015) %>% 
    select(country,year,
           `Coal and coal products#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Coal and coal products#Electricity output (GWh)-main activity producer electricity plants`)
  
  if(sum(tmp_data$GWh, na.rm=TRUE) == 0) {
    tmp_data <- tmp_data %>% 
      mutate(MW=0) %>% 
      mutate(load_factor=NA)
  } else {
    tmp_data <- tmp_data %>% 
      left_join(data_pp_other %>% 
                  filter(FUELCATEGORY == "COAL", COUNTRY == data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) == k_country)]) %>% 
                  select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
                  get_annualcap(1980:2015),
                by=c("year")) %>% 
      rename(MW=cap) %>% 
      mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
  }
  lf_data[[k_country]] <- tmp_data
}
lf_df <- do.call("rbind", lf_data)
p <- ggplot(lf_df) +
  geom_line(aes(year,load_factor,color=country))
print(p)

# All 
tmp_data <- rbind(
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Coal and coal products#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Coal and coal products#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUELCATEGORY == "COAL", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type="coal"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Crude, NGL and feedstocks#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Crude, NGL and feedstocks#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUELCATEGORY == "OIL", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "oil"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Natural gas#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Natural gas#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUELCATEGORY == "GAS", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "gas"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Nuclear#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Nuclear#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUELCATEGORY == "UR", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "nuclear"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Biofuels and waste#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Biofuels and waste#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUELCATEGORY == "BIOMASS", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "biomass"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Hydro#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Hydro#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUELCATEGORY == "HYDRO", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "hydro"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Geothermal#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Geothermal#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUEL == "GEO", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "geothermal"),
  data_wbal_ssa %>% 
    filter(country %in% u_countries, year %in% 1980:2015) %>% 
    select(country,year,
           `Solar/wind/other#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    rename(GWh=`Solar/wind/other#Electricity output (GWh)-main activity producer electricity plants`) %>% 
    group_by(year) %>% 
    summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
    ungroup %>% 
    left_join(
      data_pp_other %>% 
        filter(FUEL %in% c("SUN", "WIND"), COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
               grepl("UTIL:", BUSTYPE)) %>% 
        select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
        group_by(STATUS,YEAR,RETIRE) %>% 
        summarise(MW=sum(MW, na.rm=TRUE)) %>% 
        ungroup() %>% 
        get_annualcap(1980:2015),
      by=c("year")) %>% 
    rename(MW=cap) %>% 
    mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
    mutate(type = "solar/wind"))
p <- ggplot(tmp_data %>% filter(year >= 2010)) +
  geom_line(aes(year,load_factor)) +
  facet_wrap(~type, ncol=4, scales="free_y") +
  ylab("Load factor [%]") + xlab("")
print(p)
tmp_data %>%
  filter(year >= 2010, year <= 2015) %>% 
  group_by(type) %>% 
  summarise(load_factor=mean(load_factor)) %>% 
  ungroup()

#----- Coal -------
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Coal and coal products#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Coal and coal products#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUELCATEGORY == "COAL", 
             COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)],
             grepl("UTIL:", BUSTYPE)) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2)) %>% 
  mutate(type="coal")
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#---- Oil --------
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Crude, NGL and feedstocks#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Crude, NGL and feedstocks#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUELCATEGORY == "OIL", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#------ Gas ----------
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Natural gas#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Natural gas#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUELCATEGORY == "GAS", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#------ Nuclear ------
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Nuclear#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Nuclear#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUELCATEGORY == "UR", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#--- Biomass ----
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Biofuels and waste#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Biofuels and waste#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUELCATEGORY == "BIOMASS", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#---- Hydro  -------
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Hydro#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Hydro#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUELCATEGORY == "HYDRO", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#---  Geothermal ----
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Geothermal#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Geothermal#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUEL == "GEO", COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#---- Solar and Wind ------
tmp_data <- data_wbal_ssa %>% 
  filter(country %in% u_countries, year %in% 1980:2015) %>% 
  select(country,year,
         `Solar/wind/other#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  rename(GWh=`Solar/wind/other#Electricity output (GWh)-main activity producer electricity plants`) %>% 
  group_by(year) %>% 
  summarise(GWh=sum(GWh, na.rm=TRUE)) %>% 
  ungroup %>% 
  left_join(
    data_pp_other %>% 
      filter(FUEL %in% c("SUN", "WIND"), COUNTRY %in% data_ssa$COUNTRY_PLATTS[which(substr(data_ssa$Country, 2, nchar(data_ssa$Country)) %in% u_countries)]) %>% 
      select(UNIT,MW,STATUS,YEAR,RETIRE) %>%
      group_by(STATUS,YEAR,RETIRE) %>% 
      summarise(MW=sum(MW, na.rm=TRUE)) %>% 
      ungroup() %>% 
      get_annualcap(1980:2015),
    by=c("year")) %>% 
  rename(MW=cap) %>% 
  mutate(load_factor=round(100*GWh/(MW*1e-3*365.25*24), digits = 2))
p <- ggplot(tmp_data) +
  geom_line(aes(year,load_factor))
print(p)

#-- Which SSA country has other coal usages than man elec and what is it ------
tmp_data1 <- data_wbal_ssa %>% 
  gather(prodflow, value, -country, -year) %>% 
  filter(grepl("Coal.*transf", prodflow)) %>% 
  filter(!grepl("Memo", prodflow)) %>% 
  group_by(prodflow) %>%
  mutate(keep=any(value!=0, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(keep)

tmp_data2 <- tmp_data1 %>% 
  filter(!prodflow %in% c("TPES", "Stat. diff.")) %>% 
  group_by(prodflow, country) %>% 
  filter(value != 0, !is.na(value)) %>% 
  summarise(ys=paste0("(", paste0(year, collapse=", "), ")")) %>% 
  ungroup() %>% 
  group_by(prodflow) %>%
  mutate(cys = paste0(country, " ", ys)) %>% 
  summarise(csys=paste0(cys, collapse=", ")) %>% 
  ungroup()

tmp_data3 <- data_wbal_ssa %>% 
  filter(country == "Zimbabwe") %>% 
  rename(`Coal power plants (Autoproducer)`=`Coal and coal products#Autoproducer electricity plants (transf.)`) %>% 
  rename(`Coke ovens`=`Coal and coal products#Coke ovens (transf.)`) %>% 
  rename(`Coal power plants (Main activity)`=`Coal and coal products#Main activity producer electricity plants (transf.)`) %>% 
  gather(prodflow,value,-country,-year) %>% 
  filter(prodflow %in% c("Coal power plants (Autoproducer)", 
                         "Coke ovens", 
                         "Coal power plants (Main activity)")) 
p <- ggplot(tmp_data3 %>% 
              filter(year %in% u_years)) +
  geom_line(aes(year,value, color=prodflow))
print(p)



#-- Coal usage over time in SSA --------------
tmp_data4 <- data_wbal_ssa %>% 
  #filter(country == "Zimbabwe") %>% 
  gather(prodflow,value,-country,-year) %>%
  filter(grepl("Coal and coal products", prodflow)) %>% 
  mutate(prodflow=gsub("Coal and coal products#","",prodflow)) %>% 
  filter(prodflow %in% c('Total primary energy supply', 'Statistical differences', 'Main activity producer electricity plants (transf.)', 
                         'Autoproducer electricity plants (transf.)', 'Main activity producer CHP plants (transf.)', 'Autoproducer CHP plants (transf.)', 
                         'Main activity producer heat plants (transf.)', 'Autoproducer heat plants (transf.)', 'Heat pumps (transf.)', 
                         'Electric boilers (transf.)', 'Chemical heat for electricity production (transf.)', 'Blast furnaces (transf.)', 'Gas works (transf.)', 
                         'Coke ovens (transf.)', 'Patent fuel plants (transf.)', 'BKB/peat briquette plants (transf.)', 'Oil refineries (transf.)', 
                         'Petrochemical plants (transf.)', 'Liquefaction plants (transf.)', 'Other transformation', 'Energy industry own use', 'Losses', 
                         'Total final consumption', 'Industry', 'Iron and steel', 'Chemical and petrochemical', 'Non-ferrous metals', 'Non-metallic minerals', 
                         'Transport equipment', 'Machinery', 'Mining and quarrying', 'Food and tobacco', 'Paper, pulp and printing', 'Wood and wood products', 
                         'Construction', 'Textile and leather', 'Non-specified (industry)', 'Transport', 'World aviation bunkers', 'Domestic aviation', 'Road', 
                         'Rail', 'Pipeline transport', 'World marine bunkers', 'Domestic navigation', 'Non-specified (transport)', 'Other', 'Residential', 
                         'Commercial and public services', 'Agriculture/forestry', 'Fishing', 'Non-specified (other)', 'Non-energy use', 
                         'Non-energy use industry/transformation/energy', 'Non-energy use in transport', 'Non-energy use in other')) %>% 
  mutate(prodflow=factor(prodflow, 
                         levels=c('Total primary energy supply', 'Statistical differences', 'Main activity producer electricity plants (transf.)', 
                                  'Autoproducer electricity plants (transf.)', 'Main activity producer CHP plants (transf.)', 'Autoproducer CHP plants (transf.)', 
                                  'Main activity producer heat plants (transf.)', 'Autoproducer heat plants (transf.)', 'Heat pumps (transf.)', 
                                  'Electric boilers (transf.)', 'Chemical heat for electricity production (transf.)', 'Blast furnaces (transf.)', 'Gas works (transf.)', 
                                  'Coke ovens (transf.)', 'Patent fuel plants (transf.)', 'BKB/peat briquette plants (transf.)', 'Oil refineries (transf.)', 
                                  'Petrochemical plants (transf.)', 'Liquefaction plants (transf.)', 'Other transformation', 'Energy industry own use', 'Losses', 
                                  'Total final consumption', 'Industry', 'Iron and steel', 'Chemical and petrochemical', 'Non-ferrous metals', 'Non-metallic minerals', 
                                  'Transport equipment', 'Machinery', 'Mining and quarrying', 'Food and tobacco', 'Paper, pulp and printing', 'Wood and wood products', 
                                  'Construction', 'Textile and leather', 'Non-specified (industry)', 'Transport', 'World aviation bunkers', 'Domestic aviation', 'Road', 
                                  'Rail', 'Pipeline transport', 'World marine bunkers', 'Domestic navigation', 'Non-specified (transport)', 'Other', 'Residential', 
                                  'Commercial and public services', 'Agriculture/forestry', 'Fishing', 'Non-specified (other)', 'Non-energy use', 
                                  'Non-energy use industry/transformation/energy', 'Non-energy use in transport', 'Non-energy use in other'),
                         labels=c(c('TPES', 'Stat. diff.', 'TRANSF|Main activity producer electricity plants', 
                                    'TRANSF|Autoproducer electricity plants', 'TRANSF|Main activity producer CHP plants', 'TRANSF|Autoproducer CHP plants (transf.)', 
                                    'TRANSF|Main activity producer heat plants', 'TRANSF|Autoproducer heat plants', 'TRANSF|Heat pumps', 
                                    'TRANSF|Electric boilers', 'TRANSF|Chemical heat for electricity production', 'TRANSF|Blast furnaces', 'TRANSF|Gas works (transf.)', 
                                    'TRANSF|Coke ovens', 'TRANSF|Patent fuel plants', 'TRANSF|BKB/peat briquette plants', 'TRANSF|Oil refineries', 
                                    'TRANSF|Petrochemical plants', 'TRANSF|Liquefaction plants', 'TRANSF|Other transformation', 'Energy industry own use', 'Losses', 
                                    'FC|Total', 'FC|Industry', 'FC|Industry|Iron and steel', 'FC|Industry|Chemical and petrochemical', 'FC|Industry|Non-ferrous metals', 'FC|Industry|Non-metallic minerals', 
                                    'FC|Industry|Transport equipment', 'FC|Industry|Machinery', 'FC|Industry|Mining and quarrying', 'FC|Industry|Food and tobacco', 'FC|Industry|Paper, pulp and printing', 'FC|Industry|Wood and wood products', 
                                    'FC|Industry|Construction', 'FC|Industry|Textile and leather', 'FC|Industry|Non-specified', 'FC|Transport', 'FC|Transport|World aviation bunkers', 'FC|Transport|Domestic aviation', 'FC|Transport|Road', 
                                    'FC|Transport|Rail', 'FC|Transport|Pipeline transport', 'FC|Transport|World marine bunkers', 'FC|Transport|Domestic navigation', 'FC|Transport|Non-specified', 'FC|Other', 'FC|Residential', 
                                    'FC|Commercial and public services', 'FC|Agriculture/forestry', 'FC|Fishing', 'FC|Non-specified (other)', 'FC|Non-energy use', 
                                    'FC|Non-energy use|industry/transformation/energy', 'FC|Non-energy use|in transport', 'FC|Non-energy use|in other')))) %>% 
  mutate(prodflow=paste(prodflow)) %>% 
  group_by(prodflow,country) %>% 
  mutate(keep=any(value!=0,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(keep)

u_industrial_sectors <- c('FC|Industry|Iron and steel', 'FC|Industry|Chemical and petrochemical', 'FC|Industry|Non-ferrous metals', 'FC|Industry|Non-metallic minerals', 
                          'FC|Industry|Transport equipment', 'FC|Industry|Machinery', 'FC|Industry|Mining and quarrying', 'FC|Industry|Food and tobacco', 'FC|Industry|Paper, pulp and printing', 'FC|Industry|Wood and wood products', 
                          'FC|Industry|Construction', 'FC|Industry|Textile and leather', 'FC|Industry|Non-specified')
u_transport_sectors  <- c('FC|Transport', 'FC|Transport|World aviation bunkers', 'FC|Transport|Domestic aviation', 'FC|Transport|Road', 
                          'FC|Transport|Rail', 'FC|Transport|Pipeline transport', 'FC|Transport|World marine bunkers', 'FC|Transport|Domestic navigation', 'FC|Transport|Non-specified')
u_other_sectors      <- c('FC|Residential', 'FC|Commercial and public services', 'FC|Agriculture/forestry', 'FC|Fishing', 'FC|Non-specified (other)')
u_nonenergy_use      <- c('FC|Non-energy use|industry/transformation/energy', 'FC|Non-energy use|in transport', 'FC|Non-energy use|in other')

p <- ggplot(tmp_data4 %>% 
              filter(grepl("TRANSF|FC", prodflow)) %>%
              filter(!prodflow %in% u_industrial_sectors) %>% 
              filter(!prodflow %in% u_transport_sectors) %>% 
              filter(prodflow != "FC|Other") %>% 
              filter(prodflow != "FC|Total") %>% 
              filter(year %in% 2000:2015)) +
  geom_line(aes(year, value, color=prodflow)) +
  facet_wrap(~country, ncol=6, scales="free_y")
print(p)

#-- Coal power plants vs other over time in SSA --------------
tmp_data5 <- tmp_data4 %>% 
  filter(year < 2016) %>% 
  filter(prodflow %in% c("TPES", "Stat. diff.", "Energy industry own use", "Losses", "FC|Total") | grepl("TRANSF", prodflow))
# filter(grepl("TPES|Stat. diff.|Energy industry own use|Losses|TRANSF|FC", prodflow)) %>%
# filter(!prodflow %in% u_industrial_sectors) %>% 
# filter(!prodflow %in% u_transport_sectors) %>% 
# filter(prodflow != "FC|Other") %>% 
# filter(prodflow != "FC|Total") 

tmp_data5 <- rbind(
  tmp_data5 %>% 
    filter(prodflow %in% c("TPES")) %>% 
    select(-keep),
  tmp_data5 %>% 
    filter(prodflow %in% c("Energy industry own use")) %>% 
    mutate(value=-value) %>% 
    mutate(prodflow="Coal industry own use") %>%
    select(-keep),
  tmp_data5 %>% 
    filter(prodflow %in% c("Losses")) %>% 
    mutate(value=-value) %>% 
    select(-keep),
  tmp_data5 %>% 
    filter(prodflow %in% c("Stat. diff.")) %>% 
    mutate(value=-value) %>% 
    mutate(prodflow="Statistical differences") %>%
    select(-keep),
  tmp_data5 %>% 
    filter(prodflow == 'TRANSF|Main activity producer electricity plants') %>% 
    mutate(prodflow = "Coal power plants") %>% 
    mutate(value = -value) %>% 
    select(-keep),
  tmp_data5 %>% 
    filter(!prodflow %in% c("TPES", "Stat. diff.", 'TRANSF|Main activity producer electricity plants', "Energy industry own use", "Losses")) %>% 
    group_by(country,year) %>% 
    mutate(value=ifelse(grepl("TRANSF",prodflow), -value, value)) %>% 
    summarise(value=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(prodflow="Other coal activities") %>% 
    select(country,year,prodflow,value)) %>% 
  mutate(prodflow=factor(prodflow, 
                         levels=rev(c("Coal power plants", "Other coal activities", "Coal industry own use", "Losses", "Statistical differences", "TPES")),
                         ordered = TRUE))

# require(RColorBrewer)
# require(ggplot2)
# cols = brewer.pal(8, "Set1")

red_shades <- colorRampPalette(5,colors=c("#7E3517", "#E2A76F"))

cols[1:4] <- red_shades(4)     # Coal Status

u_colorscale_coal_usage<- c(
  "Coal power plants" = "#7E3517", 
  "Other coal activities" = "#E2A76F", 
  "Coal industry own use" = "#0000ff", 
  "Losses" = "#000000", 
  "Statistical differences" = "#ff0000", 
  "TPES" = "#00ff00"
)

p <- ggplot() +
  geom_area(aes(year, value, fill=prodflow), data=tmp_data5 %>% filter(year > 1970, prodflow!="TPES") %>% group_by(year,prodflow) %>% summarise(value=sum(value,na.rm=TRUE)) %>% ungroup()) +
  geom_line(aes(year, value), data=tmp_data5 %>% filter(year > 1970, prodflow=="TPES") %>% group_by(year,prodflow) %>% summarise(value=sum(value,na.rm=TRUE)) %>% ungroup(), color="black", lty=2, size=1.15) +
  scale_fill_manual("Coal usage", values = u_colorscale_coal_usage) +
  theme_bw() +
  theme(
    text = element_text(size=16),
    legend.position = "bottom") + 
  guides(fill=guide_legend(nrow = 2, byrow=TRUE)) + 
  xlab("") + ylab("Coal consumption [ktoe]")
print(p)
ggsave(p, filename = "../plots/Coal_consumption_SSA_by_activity.png", width=10, height=8)
ggsave(p, filename = "../plots/Coal_consumption_SSA_by_activity.svg", width=10, height=8)

p <- ggplot() +
  geom_area(aes(year, value, fill=prodflow), data=tmp_data5 %>% filter(year > 1970, prodflow!="TPES")) +
  geom_line(aes(year, value), data=tmp_data5 %>% filter(year > 1970, prodflow=="TPES"), color="black", lty=2, size=1.15) +
  facet_wrap(~ country, ncol=4, scales="free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow = 2, byrow=TRUE)) + 
  scale_fill_manual("Coal usage", values = u_colorscale_coal_usage) +
  xlab("") + ylab("Coal consumption [ktoe]")
print(p)
ggsave(p, filename = "../plots/Coal_consumption_SSA_by_activity_by_region.png", width=10, height=8)
ggsave(p, filename = "../plots/Coal_consumption_SSA_by_activity_by_region.svg", width=10, height=8)


#-- Other coal acivities over time in SSA --------------
tmp_data6 <- tmp_data4 %>% 
  filter(year < 2016) %>% 
  filter(grepl("FC", prodflow)) %>% 
  filter(prodflow != "FC|Industry") %>% 
  filter(prodflow != "FC|Transport") %>% 
  filter(prodflow != "FC|Other") %>% 
  filter(!prodflow %in% c('FC|Non-energy use|industry/transformation/energy', 'FC|Non-energy use|in transport', 'FC|Non-energy use|in other'))
# filter(grepl("TPES|Stat. diff.|Energy industry own use|Losses|TRANSF|FC", prodflow)) %>%
# filter(!prodflow %in% u_industrial_sectors) %>% 
# filter(!prodflow %in% u_transport_sectors) %>% 
# filter(prodflow != "FC|Other") %>% 
# filter(prodflow != "FC|Total") 

tmp_data6 <- rbind(
  tmp_data6 %>% 
    filter(prodflow == c("FC|Total")) %>% 
    mutate(prodflow = "Total") %>% 
    select(-keep),
  tmp_data6 %>% 
    filter(prodflow != c("FC|Total")) %>% 
    mutate(prodflow=substr(prodflow, 4, nchar(prodflow))) %>%
    select(-keep))

u_colorscale_coal_usage<- c(
  "Agriculture/Forestry" = "#00ff00", 
  "Residential" = "#0000ff", 
  "Commerical and public services" = "#0000ff", 
  
  "TPES" = "#000000"
)

u_fc_linreg_years <- 2005:2015
fc_linreg_vals <- tmp_data6 %>% 
  filter(year %in% u_fc_linreg_years, prodflow=="Total") %>% 
  group_by(year,prodflow) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(value)
res <- lm(y~x, data=data.frame(x=u_fc_linreg_years, y=fc_linreg_vals$value))
val2025 <- 2025*res$coefficients[2] + res$coefficients[1]

u_fc_linreg_years <- 2005:2015
fc_linreg_vals <- rbind(
  # # Zero: 
  # tmp_data6 %>% 
  #   filter(
  #     year %in% u_fc_linreg_years,
  #     prodflow %in% c()) %>% 
  #   group_by(year,prodflow) %>% 
  #   summarise(value=sum(value,na.rm=TRUE)) %>% 
  #   ungroup() %>% 
  #   group_by(prodflow) %>% 
  #   summarise(value = 0) %>% 
  #   ungroup() %>% 
  #   mutate(year=2025),
  # Constant:
  tmp_data6 %>% 
    filter(
      year %in% u_fc_linreg_years,
      prodflow %in% c("Agriculture/forestry", "Commercial and public services", "Industry|Chemical and petrochemical",
                      "Industry|Construction", "Industry|Iron and steel", "Industry|Machinery", "Industry|Mining and quarrying",
                      "Industry|Non-ferrous metals", "Industry|Textile and leather", "Industry|Transport equipment",
                      "Residential", "Transport|Rail")) %>% 
    group_by(year,prodflow) %>% 
    summarise(value=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>% 
    filter(year == 2015) %>% 
    mutate(year=2025),
  # Linear increase:
  tmp_data6 %>% 
    filter(
      year %in% u_fc_linreg_years,
      prodflow %in% c("Total", "Industry|Food and tobacco", "Industry|Non-metallic minerals", "Industry|Non-specified",
                      "Industry|Paper, pulp and printing", "Industry|Wood and wood products", "Non-specified (other)")) %>% 
    group_by(year,prodflow) %>% 
    summarise(value=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>% 
    group_by(prodflow) %>% 
    summarise(value = 2025*(lm(y~x, data=data.frame(x=year, y=value)))$coefficients[2] + (lm(y~x, data=data.frame(x=year, y=value)))$coefficients[1]) %>% 
    ungroup() %>% 
    mutate(year=2025))

fc_linreg_vals %>% 
  filter(prodflow != "Total") %>% 
  group_by(year) %>% 
  summarise(value=sum(value)) %>% 
  ungroup() %>% 
  mutate(value = value * 0.0000418680000)

p <- ggplot() +
  geom_area(aes(year, value, fill=prodflow), data=tmp_data6 %>% filter(year > 1970, prodflow!="Total") %>% group_by(year,prodflow) %>% summarise(value=sum(value,na.rm=TRUE)) %>% ungroup()) +
  geom_line(aes(year, value), data=tmp_data6 %>% filter(year > 1970, prodflow=="Total") %>% group_by(year,prodflow) %>% summarise(value=sum(value,na.rm=TRUE)) %>% ungroup(), color="black", lty=2, size=1.15) +
  geom_point(aes(x=year, y=value), data=fc_linreg_vals %>% filter(prodflow != "Total") %>% group_by(year) %>% summarise(value=sum(value)) %>% ungroup()) +
  geom_point(aes(x=year, y=value), data=fc_linreg_vals %>% filter(prodflow == "Total")) +
  theme_bw() +
  theme(
    text = element_text(size=16),
    legend.position = "bottom") + 
  guides(fill=guide_legend(nrow = 3, byrow=TRUE)) + 
  xlab("") + ylab("Coal consumption [ktoe]")
print(p)

p <- ggplot() +
  geom_area(aes(year, value, fill=prodflow), data=tmp_data6 %>% filter(year > 1970, prodflow!="Total") %>% group_by(year,prodflow) %>% summarise(value=sum(value,na.rm=TRUE)) %>% ungroup()) +         
  geom_point(aes(year, value), data=fc_linreg_vals %>% filter(prodflow != "Total")) +
  facet_wrap(~prodflow, ncol=6, scales="free_y") +
  theme_bw() +
  theme(
    text = element_text(size=16),
    legend.position = "bottom") + 
  guides(fill=guide_legend(nrow = 3, byrow=TRUE)) + 
  xlab("") + ylab("Coal consumption [ktoe]")
print(p)

p <- ggplot() +
  geom_area(aes(year, value, fill=prodflow), data=tmp_data6 %>% filter(year > 1970, prodflow!="Total")) +
  geom_line(aes(year, value), data=tmp_data6 %>% filter(year > 1970, prodflow=="Total"), color="black", lty=2, size=1.15) +
  facet_wrap(~ country, ncol=4, scales="free_y")  +
  theme_bw() +
  theme(
    text = element_text(size=16),
    legend.position = "bottom") + 
  guides(fill=guide_legend(nrow = 3, byrow=TRUE)) + 
  xlab("") + ylab("Coal consumption [ktoe]")
print(p)

p <- ggplot() +
  geom_area(aes(year, value, fill=prodflow), data=tmp_data6 %>% filter(year > 1970, prodflow!="Total")) +
  facet_grid(country ~ prodflow , scales="free_y")  +
  theme_bw() +
  theme(
    text = element_text(size=16),
    legend.position = "bottom") + 
  guides(fill=guide_legend(nrow = 3, byrow=TRUE)) + 
  xlab("") + ylab("Coal consumption [ktoe]")
print(p)

#-- Statistical differences over time in SSA -----------------
tmp_data8 <- data_wbal_ssa %>% 
  gather(prodflow, value, -country, -year) %>% 
  filter(grepl("Coal.*Statistical difference", prodflow)) %>% 
  filter(!grepl("Memo", prodflow)) %>% 
  group_by(country) %>% 
  mutate(keep=any(value!=0,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(keep)

p <- ggplot(tmp_data8) +
  geom_line(aes(year, value, color=country)) +
  facet_wrap(~ country, ncol=4, scales = "free_y") 
print(p)