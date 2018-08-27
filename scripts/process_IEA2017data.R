# Generate data from raw IEA csv files
u_path <- "data/IEA 2017/Africa data"

# Get data
files_pe    <- list.files(file.path(u_path, "Primary energy"), full.names = T)
files_co2   <- list.files(file.path(u_path, "CO2 emissions"), full.names = T)
files_other <- list.files(file.path(u_path, "Other data"), full.names = T)

data_iea2017 <- list()

for (kcountry in 1:33) {
  
  if (basename(files_pe[kcountry])    != basename(files_co2[kcountry]))   print(paste0("Warning! Difference filenames: ", basename(files_pe[kcountry]),    " VS ", basename(files_co2[kcountry])))
  if (basename(files_other[kcountry]) != basename(files_co2[kcountry]))   print(paste0("Warning! Difference filenames: ", basename(files_other[kcountry]), " VS ", basename(files_co2[kcountry])))
  if (basename(files_pe[kcountry])    != basename(files_other[kcountry])) print(paste0("Warning! Difference filenames: ", basename(files_pe[kcountry]),    " VS ", basename(files_other[kcountry])))
  
  # Get PE (data is in PJ)
  tmp_pe <- read.csv2(files_pe[kcountry], header = 1, skip = 2, dec=".", na.strings = "..", colClasses = rep("numeric", 20))
  tmp_pe <- tmp_pe[-1,1:16] # remove first row
  
  names(tmp_pe) <- c("TIME", "PE Coal and Coal Products", "PE Peat", "Oil shale and oil sands", "PE Crude, NGL and Feedstocks", "PE Petroleum Products", 
                     "PE Natural Gas", "PE Nuclear",  "PE Hydro", "PE Geothermal", "PE Solar/Wind/Other", "PE Combustible Renewables and Waste", 
                     "PE Heat Production from non-specified comb.fuels", "PE Electricity", "Heat", "Total Primary Energy Supply (ktoe)")
  
  # Convert from PJ to ktoe
  for(kvar in 2:ncol(tmp_pe)) {
    tmp_pe[,kvar] <- tmp_pe[,kvar] * 1/41.8680000 
  }
  
  #merge oil shale and oil sands with coal and coal products?
  tmp_pe$`PE Coal and Coal Products` <- tmp_pe$`PE Coal and Coal Products` + tmp_pe$`Oil shale and oil sands`
  tmp_pe <- tmp_pe %>% select(-`Oil shale and oil sands`)
  
  # Get CO2
  tmp_co2 <- read.csv2(files_co2[kcountry], header = 1, skip = 2, dec=",", na.strings = "..", colClasses = rep("numeric", 6))
  #read.csv2(files_co2[kcountry])
  
  tmp_co2 <- tmp_co2[-1,] # remove first row
  
  names(tmp_co2) <- c("TIME", "Total", "CO2 Coal and Coal Products", "CO2 Oil", "CO2 Natural gas", "CO2 Other")
  
  
  # Get Other
  tmp_other <- read.csv2(files_other[kcountry], header = 1, skip = 2, dec=",", na.strings = "..", colClasses = rep("numeric", 31))
  #read.csv2(files_other[kcountry])
  tmp_other <- tmp_other[-1,1:7] # remove first row and select first 7 columns
  names(tmp_other) <- c("TIME", "CO2 Reference Approach (Mt of CO2)", "Total Primary Energy Supply (PJ)", "Total Primary Energy Supply (Mtoe)",
                        "GDP (billion 2000 US$ using exchange rates)", "GDP (billion 2000 US$ using PPPs)",
                        "Population (millions)")
  
  tmp_data <-
    tmp_co2 %>% 
    left_join(tmp_other, by="TIME") %>% 
    left_join(tmp_pe, by="TIME")
  
  country <- substr(basename(files_pe[kcountry]), 1, nchar(basename(files_pe[kcountry]))-4)
  print(country)
  
  tmp_data$COUNTRY <- country
  
  data_iea2017[[country]] <- tmp_data
  
}

data_iea2017 <- do.call("rbind", data_iea2017)

tmp_data_iea2017_ssa <- data_iea2017 %>% 
  filter(TIME > 1970) %>% 
  filter(!COUNTRY %in% c("Africa", "Memo Africa UN", "Albania", "Algeria", "Mauritius", "Morocco", "Egypt", "Libya", "South Africa", "Tunisia"))

row.names(tmp_data_iea2017_ssa) <- NULL



out_data3 <- data.frame(
  "TIME"                               = tmp_data_iea2017_ssa$TIME, 
  "CO2 Coal and Coal Products"         = tmp_data_iea2017_ssa$`CO2 Coal and Coal Products`,
  "CO2 Coal - Announced"               = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "CO2 Coal - Operating"               = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "CO2 Coal - Permitted"               = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "CO2 Coal - Pre-permit development"  = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "CO2 Coal - Shelved"                 = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "CO2 Oil "                           = tmp_data_iea2017_ssa$`CO2 Oil`,
  "CO2 Natural Gas "                   = tmp_data_iea2017_ssa$`CO2 Natural gas`,
  "CO2 Other "                         = tmp_data_iea2017_ssa$`CO2 Other`,	
  "CO2 Sectoral Approach (Mt of CO2)"  = tmp_data_iea2017_ssa$Total,	
  "Coal and Coal Products"             = tmp_data_iea2017_ssa$`CO2 Coal and Coal Products`,	
  "Coal - Announced"                   = rep(0, length(tmp_data_iea2017_ssa$TIME)),	
  "Coal - Operating"                   = rep(0, length(tmp_data_iea2017_ssa$TIME)),	
  "Coal - Permitted"                   = rep(0, length(tmp_data_iea2017_ssa$TIME)),	
  "Coal - Pre-permit development"      = rep(0, length(tmp_data_iea2017_ssa$TIME)),	
  "Coal - Shelved"                     = rep(0, length(tmp_data_iea2017_ssa$TIME)),	
  "Oil "                               = tmp_data_iea2017_ssa$`CO2 Oil`, 	
  "Natural Gas "                       = tmp_data_iea2017_ssa$`CO2 Natural gas`, 	
  "Other "                             = tmp_data_iea2017_ssa$`CO2 Other`, 	
  "CO2 Reference Approach (Mt of CO2)" = tmp_data_iea2017_ssa$Total,	
  "Total Primary Energy Supply (PJ)"   = tmp_data_iea2017_ssa$`Total Primary Energy Supply (PJ)`,	
  "Total Primary Energy Supply (Mtoe)" = tmp_data_iea2017_ssa$`Total Primary Energy Supply (Mtoe)`,	
  "GDP (billion 2000 US$ using exchange rates)" = tmp_data_iea2017_ssa$`GDP (billion 2000 US$ using exchange rates)`,	
  "GDP (billion 2000 US$ using PPPs)"           = tmp_data_iea2017_ssa$`GDP (billion 2000 US$ using PPPs)`,	
  "Population (millions)"                       = tmp_data_iea2017_ssa$`Population (millions)`,
  "PE Coal and Coal Products"                   = tmp_data_iea2017_ssa$`PE Coal and Coal Products`,
  "PE Coal - Announced"                         = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "PE Coal - Operating"                         = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "PE Coal - Permitted"                         = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "PE Coal - Pre-permit development"            = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "PE Coal - Shelved"                           = rep(0, length(tmp_data_iea2017_ssa$TIME)),
  "PE Peat"                                     = tmp_data_iea2017_ssa$`PE Peat`,	
  "PE Crude, NGL and Feedstocks"                = tmp_data_iea2017_ssa$`PE Crude, NGL and Feedstocks`, 
  "PE Petroleum Products"                       = tmp_data_iea2017_ssa$`PE Petroleum Products`,	
  "PE Natural Gas"                              = tmp_data_iea2017_ssa$`PE Natural Gas`, 
  "PE Nuclear"                                  = tmp_data_iea2017_ssa$`PE Nuclear`,	
  "PE Hydro"                                    = tmp_data_iea2017_ssa$`PE Hydro`,	
  "PE Geothermal"                               = tmp_data_iea2017_ssa$`PE Geothermal`,	
  "PE Solar/Wind/Other"                         = tmp_data_iea2017_ssa$`PE Solar/Wind/Other`,	
  "PE Combustible Renewables and Waste"         = tmp_data_iea2017_ssa$`PE Combustible Renewables and Waste`,	
  "PE Heat Production from non-specified comb.fuels" = tmp_data_iea2017_ssa$`PE Heat Production from non-specified comb.fuels`,
  "PE Electricity"                              = tmp_data_iea2017_ssa$`PE Electricity`,
  "Heat"                                        = tmp_data_iea2017_ssa$Heat,
  "Total Primary Energy Supply (ktoe)"          = tmp_data_iea2017_ssa$`Total Primary Energy Supply (ktoe)`,
  "COUNTRY"                                     = tmp_data_iea2017_ssa$COUNTRY
) 

var_names_extended = c(
  "year",
  "CO2 Coal and Coal Products", "CO2 Coal - Announced", "CO2 Coal - Operating", "CO2 Coal - Permitted", "CO2 Coal - Pre-permit development", "CO2 Coal - Shelved",
  "CO2 Oil ","CO2 Natural Gas ","CO2 Other ","CO2 Sectoral Approach (Mt of CO2)",
  "Coal and Coal Products", "Coal - Announced", "Coal - Operating", "Coal - Permitted", "Coal - Pre-permit development", "Coal - Shelved",
  "Oil ","Natural Gas ","Other ","CO2 Reference Approach (Mt of CO2)",
  "Total Primary Energy Supply (PJ)","Total Primary Energy Supply (Mtoe)","GDP (billion 2000 US$ using exchange rates)","GDP (billion 2000 US$ using PPPs)",
  "Population (millions)",
  "PE Coal and Coal Products", "PE Coal - Announced", "PE Coal - Operating", "PE Coal - Permitted", "PE Coal - Pre-permit development", "PE Coal - Shelved",
  "PE Peat","PE Crude, NGL and Feedstocks","PE Petroleum Products","PE Natural Gas",
  "PE Nuclear","PE Hydro","PE Geothermal","PE Solar/Wind/Other","PE Combustible Renewables and Waste",
  "PE Heat Production from non-specified comb.fuels","PE Electricity","Heat","Total Primary Energy Supply (ktoe)")  

data_iea2017_ssa <- out_data3 %>% 
  gather(variable, value, -TIME, -COUNTRY) %>% 
  group_by(TIME, variable) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(variable, value, convert=FALSE) 


data_iea2017_ssa <- data_iea2017_ssa[,names(out_data3)[-which(names(out_data3) == "COUNTRY")]]

names(data_iea2017_ssa) <- var_names_extended

save(data_iea2017_ssa, file="data/IEA 2017/data_iea2017_ssa.RData")

names(out_data3) <- c(
  "year",
  "CO2 Coal and Coal Products", "CO2 Coal - Announced", "CO2 Coal - Operating", "CO2 Coal - Permitted", "CO2 Coal - Pre-permit development", "CO2 Coal - Shelved",
  "CO2 Oil ","CO2 Natural Gas ","CO2 Other ","CO2 Sectoral Approach (Mt of CO2)",
  "Coal and Coal Products", "Coal - Announced", "Coal - Operating", "Coal - Permitted", "Coal - Pre-permit development", "Coal - Shelved",
  "Oil ","Natural Gas ","Other ","CO2 Reference Approach (Mt of CO2)",
  "Total Primary Energy Supply (PJ)","Total Primary Energy Supply (Mtoe)","GDP (billion 2000 US$ using exchange rates)","GDP (billion 2000 US$ using PPPs)",
  "Population (millions)",
  "PE Coal and Coal Products", "PE Coal - Announced", "PE Coal - Operating", "PE Coal - Permitted", "PE Coal - Pre-permit development", "PE Coal - Shelved",
  "PE Peat","PE Crude, NGL and Feedstocks","PE Petroleum Products","PE Natural Gas",
  "PE Nuclear","PE Hydro","PE Geothermal","PE Solar/Wind/Other","PE Combustible Renewables and Waste",
  "PE Heat Production from non-specified comb.fuels","PE Electricity","Heat","Total Primary Energy Supply (ktoe)", "COUNTRY")  
# Write out data for Jan
table_name   = c("Angola", "Benin", "Botswana", "Cameroon", "Congo", "Cote d'Ivoire", "Democratic Republic of the Congo", "Ethiopia", "Eritrea",
                 "Gabon", "Ghana", "Kenya", "Mozambique", "Namibia", "Niger", "Nigeria", "Senegal", "Sudan", "South Sudan", "Tanzania", "Togo", "Zambia", "Zimbabwe",
                 "Other Africa")
csv_filename = c("Angola", "Benin", "Botswana", "Cameroon", "Congo", "Cote_d_Ivoire", "Democratic_Republic_of_Congo", "Ethiopia", "Eritrea",
                 "Gabon", "Ghana", "Kenya", "Mozambique", "Namibia", "Niger", "Nigeria", "Senegal", "Sudan", "South_Sudan", "Tanzania", "Togo", "Zambia", "Zimbabwe",
                 "Other_Africa")
for (kcountry in table_name) {
  tmp_data <- out_data3 %>% 
    filter(COUNTRY == kcountry) %>% 
    select(-COUNTRY, 
           -`CO2 Coal - Announced`, -`CO2 Coal - Operating`, -`CO2 Coal - Permitted`, -`CO2 Coal - Pre-permit development`, -`CO2 Coal - Shelved`,
           -`Coal - Announced`, -`Coal - Operating`, -`Coal - Permitted`, -`Coal - Pre-permit development`, -`Coal - Shelved`,
           -`PE Coal - Announced`, -`PE Coal - Operating`, -`PE Coal - Permitted`, -`PE Coal - Pre-permit development`, -`PE Coal - Shelved`)
  
  #names(tmp_data) <- var_names_extended
  
  write.csv(tmp_data, 
            append = FALSE, row.names = FALSE,
            file = paste0("X:/stej/Renaissance of Coal/Africa/WEB 2017/Processed files/", csv_filename[which(table_name == kcountry)], ".csv"))
}


# Check population levels with SSP data
data_pop_iea <- tmp_data_iea2017_ssa %>% 
  select(COUNTRY, TIME, `Population (millions)`) %>% 
  rename(country=COUNTRY, year=TIME, pop_IEA=`Population (millions)`) 

data_pop_ssp <- readxl::read_excel("ssp_pop_data.xlsx") %>% 
  dplyr::select(-variable, -unit) %>% 
  tidyr::gather(year, pop_SSP, -iso) %>% 
  dplyr::mutate(year=as.numeric(year))

data_pop <- data_ssa %>% 
  dplyr::select(-COUNTRY_PLATTS, -COUNTRY_COALSWARM) %>% 
  dplyr::mutate(Country=substr(Country, 2, nchar(Country))) %>% 
  dplyr::mutate(Country=ifelse(Country == "Republic of the Congo", "Congo", Country)) %>% 
  dplyr::mutate(Country=ifelse(Country == "Ivory Coast", "Cote d'Ivoire", Country)) %>% 
  dplyr::left_join(data_pop_iea %>% 
                     filter(country != "Other Africa"), by=c("Country"="country"))%>% 
  dplyr::right_join(data_pop_ssp, by=c("ISO"="iso", "year")) %>% 
  dplyr::filter(year >= 1990, year <= 2030) %>% 
  dplyr::filter(Country %in% unique(data_pop_iea$country))

ggplot(data_pop %>% tidyr::gather(source, value, -Country, -ISO, -year)) + 
  geom_line(aes(x=year, y=value, color=source)) + 
  geom_line(aes(x=year, y=value, color=source), 
            data=data_pop %>% tidyr::gather(source, value, -Country, -ISO, -year) %>% dplyr::filter(!is.na(value), source=="ssppop")) +
  geom_point(aes(x=year, y=value, color=source)) +
  facet_wrap(~ ISO, scales = "free_y", ncol=6) + 
  theme_bw() + ylab("Population [million]") + xlab("")

