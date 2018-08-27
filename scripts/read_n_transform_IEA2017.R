# Transform IEA raw csv files to RData files
library(dplyr)
library(tidyr)
library(ggplot2)

u_countries           <- c('Angola', 'Benin', 'Botswana', 'Cameroon', 'Congo', "Côte d'Ivoire", 'Democratic Republic of the Congo', 
                           'Eritrea', 'Ethiopia', 'Gabon', 'Ghana', 'Kenya', 'Mozambique', 'Namibia', 'Niger', 
                           'Nigeria', 'Senegal', 'South Sudan', 'Sudan', 'Tanzania', 'Togo', 'Zambia', 'Zimbabwe', 'Other Africa')

# World Energy Balances
data_wbal   <- read.csv2("data/IEA 2017/wbal_all_ktoe.csv", stringsAsFactors = FALSE)
ncols       <- ncol(data_wbal)
nrows       <- nrow(data_wbal)

products_names <- c("Coal and coal products", "Peat and peat products", "Oil shale and oil sands", "Crude, NGL and feedstocks",
                    "Oil products", "Natural gas", "Nuclear", "Hydro", "Geothermal", "Solar/wind/other", "Biofuels and waste",
                    "Heat production from non-specified combustible fuels", "Electricity", "Heat", "Total", 
                    "Memo: Renewables", "Memo: Coal, peat and oil shale", "Memo: Primary and secondary oil", 
                    "Memo: Geothermal, solar/wind/other, heat, electricity")
flows_names <- trimws(unique(paste(data_wbal[1, 3:ncols])))

products_flows_names <- unlist(lapply(products_names, function(x) {paste0(x, "#", flows_names)}))

tmp_data_wbal <- data_wbal[4:nrows, 1:ncols]
names(tmp_data_wbal) <- c("country", "year", products_flows_names)
for(kcol in 2:ncols) {
  tmp_data_wbal[,kcol] <- as.numeric(tmp_data_wbal[,kcol])
}

data_wbal <- tmp_data_wbal %>% 
  gather(variable,value,-country,-year) %>% 
  separate(variable, into = c("product", "flow"), sep="#")


save(data_wbal, file = "data/IEA 2017/data_wbal.RData")


# World Energy Balances (Indicators)
# 1st Row   : Flows
# 1st Column: Countries
# 2nd Column: Years
data_wind     <- read.csv2("data/IEA 2017/wind.csv", 
                           header = TRUE, dec=",", na.strings = c("..", "x"),
                           colClasses = c("character", "character", rep("numeric", 50)),
                           stringsAsFactors = FALSE)
data_wind2    <- read.csv2("data/IEA 2017/wind.csv", 
                           header = FALSE, stringsAsFactors = FALSE)
ncols         <- ncol(data_wind)
nrows         <- nrow(data_wind)
tmp_data_wind <- data_wind[2:nrows,1:ncols]
countries     <- data_wind[2:nrows,1]
years         <- data_wind[2:nrows,2]
flows         <- data_wind2[1,3:ncols]
names(tmp_data_wind) <- c("country", "year", paste(flows))
data_wind <- tmp_data_wind %>% 
  tidyr::gather(flow, value, -country, -year)

save(data_wind, file = "data/IEA 2017/data_wind.RData")


# CO2 Emissions from combustion
data_wco2     <- read.csv2("data/IEA 2017/wco2.csv", 
                           header = TRUE, dec=",", na.strings = c("..", "x"),
                           colClasses = c("character", "character", "character", rep("numeric", 12)),
                           stringsAsFactors = FALSE)
data_wco22    <- read.csv2("data/IEA 2017/wco2.csv", 
                           header = FALSE, stringsAsFactors = FALSE)
ncols         <- ncol(data_wco2)
nrows         <- nrow(data_wco2)
tmp_data_wco2 <- data_wco2[2:nrows,1:ncols]
countries     <- data_wco2[2:nrows,1]
years         <- data_wco2[2:nrows,2]
products      <- data_wco2[2:nrows,3]
flows         <- data_wco22[1,4:ncols]
names(tmp_data_wco2) <- c("country", "year", "product", paste(flows))
data_wco2 <- tmp_data_wco2 %>% 
  tidyr::gather(flow, value, -country, -year, -product)


u_product_categories <- list(
  "Total"                     = c('Total'),
  "Coal and coal products"    = c('Hard coal (if no detail)', 'Brown coal (if no detail)', 'Anthracite', 'Coking coal', 'Other bituminous coal', 'Sub-bituminous coal', 'Lignite', 'Patent fuel', 'Coke oven coke', 'Gas coke', 'Coal tar', 'BKB', 'Gas works gas', 'Coke oven gas', 'Blast furnace gas', 'Other recovered gases'),
  "Peat and peat products"    = c('Peat', 'Peat products'),
  "Oil shale and oil sands"   = c('Oil shale'),
  "Natural gas"               = c("Natural gas"),
  "Crude, NGL and feedstocks" = c('Crude/NGL/feedstocks (if no detail)', 'Crude oil', 'Natural gas liquids', 'Refinery feedstocks', 'Additives/blending components', 'Orimulsion', 'Other hydrocarbons'),
  "Oil products"              = c('Refinery gas', 'Ethane', 'Liquefied petroleum gases (LPG)', 'Motor gasoline excl. biofuels', 'Aviation gasoline', 'Gasoline type jet fuel', 'Kerosene type jet fuel excl. biofuels', 'Other kerosene', 'Gas/diesel oil excl. biofuels', 'Fuel oil', 'Naphtha', 'White spirit & SBP', 'Lubricants', 'Bitumen', 'Paraffin waxes', 'Petroleum coke', 'Non-specified oil products'),
  "Biofuels and waste"        = c('Industrial waste', 'Municipal waste (non-renew)')
)
data_wco2     <- read.csv2("data/IEA 2017/wbigco2.csv", 
                           header = TRUE, dec=",", na.strings = c("..", "x"),
                           colClasses = c("character", "character", "character", rep("numeric", 41)),
                           stringsAsFactors = FALSE)
data_wco22    <- read.csv2("data/IEA 2017/wbigco2.csv", 
                           header = FALSE, stringsAsFactors = FALSE)
ncols         <- ncol(data_wco2)
nrows         <- nrow(data_wco2)
tmp_data_wco2 <- data_wco2[2:nrows,1:ncols]
countries     <- data_wco2[2:nrows,1]
years         <- data_wco2[2:nrows,2]
products      <- data_wco2[2:nrows,3]
flows         <- data_wco22[1,4:ncols]
names(tmp_data_wco2) <- c("country", "year", "product", paste(flows))
data_wco2 <- tmp_data_wco2 %>% 
  tidyr::gather(flow, value, -country, -year, -product)

data_wco2 <- lapply(1:length(u_product_categories),
                    function(x) {
                      category_name = names(u_product_categories)[x]
                      cur_prod <- u_product_categories[[x]]
                      out <- data_wco2 %>% 
                        dplyr::filter(product %in% cur_prod) %>% 
                        dplyr::group_by(country,year,flow) %>% 
                        dplyr::summarise(value=sum(value, na.rm=TRUE)) %>% 
                        dplyr::ungroup() %>% 
                        dplyr::mutate(product=category_name) %>% 
                        dplyr::select(country,year,product,flow,value)
                      return(out)
                    }) %>% 
  do.call("rbind", .) %>% 
  mutate(year = as.numeric(year))

save(data_wco2, file = "data/IEA 2017/data_wco2.RData")


rm("countries", "ncols", "nrows", "products", "years", "data_wco22", "flows", "data_wind2", "tmp_data_wind", "tmp_data_wco2", "u_product_categories", "tmp_data_wbal","flows_names", "kcol","products_flows_names","products_names")



data_wind %>% 
  filter(flow %in% c(
    "Total primary energy supply (TPES) (Mtoe)",
    "Total final consumption (TFC) (Mtoe)",
    "Population (millions)",
    "GDP (billion 2010 USD using exchange rates)",
    "GDP (billion 2010 USD using PPPs)"))

products <- c(
  "coal" = "Coal and coal products",
  "peat" = "Peat and peat products",
  "shale" = "Oil shale and oil sands",
  "crude" = "Crude, NGL and feedstocks",
  "oilprod" = "Oil products",
  "natgas" = "Natural gas",
  "nuclear" = "Nuclear",
  "hydro" = "Hydro",
  "geo" = "Geothermal",
  "solwin" = "Solar/wind/other",
  "biowas" = "Biofuels and waste",
  "heat2" = "Heat production from non-specified combustible fuels" ,
  "elec" = "Electricity",
  "heat" = "Heat",
  "total" = "Total"
)


tmp_data_wbal <- data_wbal %>% 
  filter(!grepl("Memo", product)) %>% 
  filter(flow %in% c(
    "Total primary energy supply",
    "Statistical differences",
    "Main activity producer electricity plants (transf.)",
    "Autoproducer electricity plants (transf.)",
    "Main activity producer CHP plants (transf.)",
    "Autoproducer CHP plants (transf.)",
    "Main activity producer heat plants (transf.)",
    "Autoproducer heat plants (transf.)",
    "Heat pumps (transf.)",
    "Electric boilers (transf.)",
    "Chemical heat for electricity production (transf.)",
    "Blast furnaces (transf.)",
    "Gas works (transf.)",
    "Coke ovens (transf.)",
    "Patent fuel plants (transf.)",
    "BKB/peat briquette plants (transf.)",
    "Oil refineries (transf.)",
    "Petrochemical plants (transf.)",
    "Liquefaction plants (transf.)",
    "Other transformation",
    "Energy industry own use",
    "Losses",
    "Total final consumption"))

tmp_data_wbal <- rbind(
  tmp_data_wbal %>% 
    filter(flow %in% c("Total primary energy supply")) %>% 
    mutate(flow = "PE total"),
  tmp_data_wbal %>% 
    filter(flow %in% c("Main activity producer electricity plants (transf.)")) %>% 
    mutate(value=-value) %>% 
    mutate(flow = "PE power plant"),
  tmp_data_wbal %>% 
    filter(!flow %in% c("Total primary energy supply", "Main activity producer electricity plants (transf.)")) %>% 
    mutate(value=ifelse(grepl("transf", flow)| flow %in% c("Other transformation",
                                                           "Energy industry own use",
                                                           "Losses"), -value, value)) %>% 
    group_by(country,year,product) %>% 
    summarise(value=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(flow="PE other") %>% 
    select(country,year,product,flow,value)) %>% 
  mutate(product = paste(factor(product, levels=paste(products), labels=names(products)))) %>% 
  filter(country %in% u_countries) %>% 
  group_by(year,product,flow) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  ungroup()

tmp_data_wbal <- rbind(
  tmp_data_wbal %>% 
    filter(!product %in% c("crude", "oilprod")),
  tmp_data_wbal %>% 
    filter(product %in% c("crude", "oilprod")) %>% 
    group_by( year, flow) %>% 
    summarise(value=sum(value, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(product="oil") %>% 
    select(year,product,flow,value)) %>% 
  filter(!product %in% c("shale", "heat2", "nuclear"))

ggplot() + 
  geom_area(aes(x=year, y=value, fill=flow),
            data=tmp_data_wbal %>% 
              filter(
                #country == "World",
                flow != "PE total")) +
  geom_line(aes(x=year, y=value),
            data=tmp_data_wbal %>% 
              filter(
                #country == "World",
                flow == "PE total")) +
  facet_wrap(~product, ncol=6, scales="free_y")



products <- c(
  "coal" = "Coal and coal products",
  "peat" = "Peat and peat products",
  "shale" = "Oil shale and oil sands",
  "crude" = "Crude, NGL and feedstocks",
  "oilprod" = "Oil products",
  "natgas" = "Natural gas",
  "biowas" = "Biofuels and waste",
  "total" = "Total"
)


tmp_data_wco2 <- data_wco2 %>% 
  filter(flow %in% c(
    "CO2 Fuel Combustion",
    "Main activity electricity plants",
    "Autoproducer electricity plants",
    "Main activity heat plants",
    "Autoproducer heat plants",
    "Main activity CHP plants",
    "Autoproducer CHP plants",
    "Unallocated autoproducers",
    "Own use in electricity, CHP and heat plants",
    "Other energy industry own use",
    "Memo: Total final consumption"))

tmp_data_wco2 <- rbind(
  tmp_data_wco2 %>% 
    filter(flow %in% c("CO2 Fuel Combustion")) %>% 
    mutate(flow = "CO2 total"),
  tmp_data_wco2 %>% 
    filter(flow %in% c("Main activity electricity plants")) %>% 
    #mutate(value=-value) %>% 
    mutate(flow = "CO2 power plant"),
  tmp_data_wco2 %>% 
    filter(!flow %in% c("CO2 Fuel Combustion", "Main activity electricity plants")) %>% 
    # mutate(value=ifelse(grepl("transf", flow)| flow %in% c("Other transformation",
    #                                                        "Energy industry own use",
    #                                                        "Losses"), -value, value)) %>% 
    group_by(country,year,product) %>% 
    summarise(value=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(flow="CO2 other") %>% 
    select(country,year,product,flow,value)) %>% 
  mutate(product = paste(factor(product, levels=paste(products), labels=names(products)))) %>% 
  filter(country %in% u_countries) %>% 
  group_by(year,product,flow) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

tmp_data_wco2 <- rbind(
  tmp_data_wco2 %>% 
    filter(!product %in% c("crude", "oilprod")),
  tmp_data_wco2 %>% 
    filter(product %in% c("crude", "oilprod")) %>% 
    group_by(year, flow) %>% 
    summarise(value=sum(value, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(product="oil") %>% 
    select(year,product,flow,value)) %>% 
  filter(!product %in% c("biowas", "peat", "shale"))

ggplot() + 
  geom_area(aes(x=year, y=value, fill=flow),
            data=tmp_data_wco2 %>% 
              filter(
                #country == "World",
                flow != "CO2 total")) +
  geom_line(aes(x=year, y=value),
            data=tmp_data_wco2 %>% 
              filter(
                #country == "World",
                flow == "CO2 total")) +
  facet_wrap(~product, ncol=6, scales="free_y")


data_all <- tmp_data_wbal %>%
  mutate(prodflow=paste0(flow, " - ", product)) %>% 
  select(-flow, -product) %>% 
  spread(prodflow, value) %>% 
  left_join(
    tmp_data_wco2 %>% 
      mutate(prodflow=paste0(flow, " - ", product)) %>% 
      select(-flow, -product) %>% 
      spread(prodflow, value),
    by=c("year")) %>% 
  left_join(
    data_wind %>% 
      filter(flow %in% c(
        "Total primary energy supply (TPES) (Mtoe)",
        "Total final consumption (TFC) (Mtoe)",
        "Population (millions)",
        "GDP (billion 2010 USD using exchange rates)",
        "GDP (billion 2010 USD using PPPs)")) %>% 
      mutate(year=as.numeric(year)) %>%
      filter(country %in% u_countries) %>% 
      group_by(year,flow) %>% 
      summarise(value=sum(value, na.rm=TRUE)) %>% 
      ungroup() %>% 
      spread(flow,value),
    by=c("year"))

data_all_coalcats <- data_all %>% 
  rename(`PE power plant - coal (operating)`=`PE power plant - coal`) %>% 
  mutate(`PE power plant - coal (construction)` = NA) %>% 
  mutate(`PE power plant - coal (pre-construction)` = NA) %>% 
  mutate(`PE power plant - coal (shelved)` = NA) %>%
  rename(`CO2 power plant - coal (operating)`=`CO2 power plant - coal`) %>% 
  mutate(`CO2 power plant - coal (construction)` = NA) %>% 
  mutate(`CO2 power plant - coal (pre-construction)` = NA) %>% 
  mutate(`CO2 power plant - coal (shelved)` = NA)

#-- SSA data in IEA2017 -----
data_all_coalcats_ssa <- data_all_coalcats %>% 
  mutate(country = "SSA") %>% 
  rename(`GDP PPP` = `GDP (billion 2010 USD using PPPs)`) %>% 
  rename(`GDP MER` = `GDP (billion 2010 USD using exchange rates)`) %>% 
  rename(`POP` = `Population (millions)`) %>% 
  mutate(`TPES` = `Total primary energy supply (TPES) (Mtoe)`*1e3) %>% 
  mutate(`TFC` = `Total final consumption (TFC) (Mtoe)`*1e3) %>% 
  select(-`Total primary energy supply (TPES) (Mtoe)`, -`Total final consumption (TFC) (Mtoe)`) %>% 
  mutate(`PE power plant - peat` = 0.0) %>% 
  mutate(`PE total - peat` = 0.0) %>% 
  mutate(`PE power plant - nuclear` = 0.0) %>% 
  mutate(`PE other - nuclear` = 0.0) %>% 
  mutate(`PE total - nuclear` = 0.0) %>% 
  mutate(`CO2 total - other` = 0.0)

# Compute emission factors from IEA data
ef_all<- tmp_data_wbal %>%
  filter(product %in% c("coal", "oil", "natgas")) %>% 
  spread(flow, value) %>% 
  left_join(
    tmp_data_wco2 %>% 
      filter(product %in% c("coal", "oil", "natgas")) %>% 
      spread(flow, value),
    by=c("year","product")) %>% 
  mutate(
    ef_pp    = `CO2 power plant`/`PE power plant`, 
    ef_other = `CO2 other`/`PE other`, 
    ef_tot   = `CO2 total`/`PE total`) %>% 
  select(-`CO2 power plant`, -`PE power plant`, -`CO2 other`, -`PE other`, -`CO2 total`, -`PE total`) %>% 
  gather(category, value, -year, -product) 

ggplot() +
  geom_line(aes(x=year, y=value), 
            data=ef_all %>% 
              filter(year > 2000)) +
  facet_grid(category~product)

ef_all_mean2000_2015 <- ef_all %>% 
  group_by(product, category) %>% 
  summarise(value=mean(value, na.rm=TRUE)) %>% 
  ungroup()

rm("products", "u_countries", "tmp_data_wbal", "tmp_data_wco2")

