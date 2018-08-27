# Load libraries
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(remind)
library(luplot)
library(directlabels)

# Own functions
grep2       <- function(v,p, ...) {return(grep(p,v, ...))}
compute_coaldata  <- function(iDataCoal, iLF, iEff, iEF) {
  out <- iDataCoal %>% # PLATTS data
    dplyr::select(Country, Plant, Status, `Capacity (MW)`) %>%
    dplyr::rename(country=Country, plant=Plant, status=Status, cap=`Capacity (MW)`) %>%
    dplyr::group_by(country, status) %>%
    dplyr::summarise(cap=sum(cap)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(status,cap) %>% 
    tidyr::gather(status,cap, -country) %>% 
    dplyr::mutate(cap_bkp=cap) %>% 
    dplyr::mutate(cap = cap * iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe) %>%
    dplyr::mutate(emi = cap * iEF) %>%
    dplyr::mutate(status=paste(status)) %>%
    dplyr::mutate(status=ifelse(status %in% c("Announced", "Permitted", "Pre"), "Pre-Construction",status)) %>%
    dplyr::group_by(country, status) %>%
    dplyr::summarise(cap=sum(cap, na.rm=TRUE), emi = sum(emi, na.rm=TRUE), cap_bkp=sum(cap_bkp, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    # Update values with latest Boom and Bust data
    left_join(readxl::read_xlsx("data/Shearer et al 2018 - Coal Africa.xlsx") %>% 
                rename(country=Country) %>% 
                gather(status,cap_bb2018, -country) %>% 
                dplyr::mutate(cap_bb2018_bkp = cap_bb2018) %>% 
                dplyr::mutate(cap_bb2018 = cap_bb2018 * iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe) %>%
                dplyr::mutate(emi_bb2018 = cap_bb2018 * iEF),
              by=c("country", "status")) %>% 
    dplyr::mutate(cap_bb2018=ifelse(!is.na(cap_bb2018), cap_bb2018, 0)) %>% 
    dplyr::mutate(emi_bb2018=ifelse(!is.na(emi_bb2018), emi_bb2018, 0)) %>% 
    dplyr::mutate(cap_bkp=ifelse(!is.na(cap_bb2018), cap_bb2018_bkp, cap_bkp)) %>% 
    dplyr::mutate(cap=ifelse(!is.na(cap_bb2018), cap_bb2018, cap)) %>% 
    dplyr::mutate(emi=ifelse(!is.na(emi_bb2018), emi_bb2018, emi)) %>% 
    dplyr::select(-cap_bb2018, -emi_bb2018, -cap_bkp, -cap_bb2018_bkp) %>% 
    # Add ivory coast and Niger data
    dplyr::add_row(country="Ivory Coast", status="Pre-Construction", 
                   cap=700*iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe, 
                   emi=700*iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe * iEF) %>% 
    dplyr::add_row(country="Ivory Coast", status="Construction", cap=0, emi=0) %>% 
    dplyr::add_row(country="Ivory Coast", status="Operating", cap=0, emi=0) %>%
    dplyr::add_row(country="Ivory Coast", status="Shelved", cap=0, emi=0) %>%
    dplyr::add_row(country="Ivory Coast", status="Retired", cap=0, emi=0) %>%
    dplyr::add_row(country="Ivory Coast", status="Cancelled", cap=0, emi=0) %>%
    dplyr::add_row(country="Niger", status="Pre-Construction", 
                   cap=600*iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe, 
                   emi=600*iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe * iEF) %>% 
    dplyr::add_row(country="Niger", status="Construction", cap=0, emi=0) %>% 
    dplyr::add_row(country="Niger", status="Operating", cap=0, emi=0) %>%
    dplyr::add_row(country="Niger", status="Shelved", cap=0, emi=0) %>%
    dplyr::add_row(country="Niger", status="Retired", cap=0, emi=0) %>%
    dplyr::add_row(country="Niger", status="Cancelled", cap=0, emi=0) %>%
    #dplyr::add_row(country="SSA",   status="Other activities", cap=0.0989, emi=0.0989 * iEF) %>% 
    dplyr::mutate(status=factor(status,
                                levels=c("Cancelled","Construction","Operating","Pre-Construction","Shelved", "Retired"),
                                labels=c(paste0("coal_", c("Ca", "C", "O", "P", "S", "R"))))) %>%
    dplyr::filter(!status %in% c("coal_Ca")) #,"coal_R"
  
  # Remove NAs
  out[which(is.na(out), arr.ind = TRUE)] = 0
  
  # Bind energy and emission data by column
  out = cbind(out %>% 
                dplyr::select(country,status,cap) %>% 
                dplyr::mutate(status=paste0(status, "_pe")) %>% 
                tidyr::spread(status,cap),
              out %>% 
                dplyr::select(country,status,emi) %>% 
                dplyr::rename(country2=country) %>% 
                dplyr::mutate(status=paste0(status, "_emi")) %>%
                tidyr::spread(status,emi))
  # Check that countries match
  if (any(out$country != out$country2)) stop()  
  out <- out %>% 
    dplyr::select(-country2)
  
  # Remove NAs
  out[which(is.na(out), arr.ind = TRUE)] = 0
  
  return(out)
}
compute_otherdata <- function(iDataOther, iFuel, iLF, iEff, iEF, iCountry) {
  out <- iDataOther %>% 
    dplyr::select(COUNTRY, PLANT, STATUS, MW) %>% 
    dplyr::rename(country=COUNTRY, plant=PLANT, status=STATUS, cap=MW) %>%            #   W >        J/yr > EJ/yr
    dplyr::mutate(cap = cap * iLF / iEff * 1e6 * 3600*24*365 * 1e-18 * EJ_2_ktoe) %>% # ktoe/yr
    dplyr::mutate(emi = cap * iEF) %>%                                                # MtCO2/yr
    dplyr::group_by(country, status) %>% 
    dplyr::summarise(cap=sum(cap), emi = sum(emi)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(status=paste(status)) %>% 
    dplyr::mutate(status=ifelse(status == "CAN", "Cancelled",
                                ifelse(status %in% c("CON", "con"), "Construction",
                                       ifelse(status %in% c("OPR", "opr"), "Operating",
                                              ifelse(status %in% c("PLN", "pln"), "Permitted",
                                                     ifelse(status %in% c("DEF", "DEL", "UNK"), "Shelved",
                                                            ifelse(status %in% c("DAC", "STN", "RET"), "Retired",status))))))) %>% 
    dplyr::mutate(status=factor(status, 
                                levels=c("Cancelled","Construction","Operating","Permitted","Shelved", "Retired"), 
                                labels=c(paste0(iFuel,"_", c("Ca", "C", "O", "P", "S", "R"))))) %>% 
    dplyr::group_by(country, status) %>% 
    dplyr::summarise(cap=sum(cap), emi = sum(emi)) %>% 
    dplyr::ungroup() %>% 
    #dplyr::mutate(status=paste0(status)) %>% 
    dplyr::filter(!status %in% c(paste0(iFuel, "_Ca")))    #,"oil_R"
  
  out <- iCountry %>% 
    dplyr::select(iso, COUNTRY_PLATTS) %>% 
    dplyr::left_join(out, 
                     by=c("COUNTRY_PLATTS"="country")) %>% 
    dplyr::filter(!is.na(status))
  out[which(is.na(out), arr.ind = TRUE)] = 0
  
  out = cbind(out %>% 
                dplyr::select(iso,status,cap) %>% 
                dplyr::mutate(status=paste0(status, "_pe")) %>% 
                spread(status,cap),
              out %>% 
                dplyr::select(iso,status,emi) %>% 
                dplyr::rename(iso2=iso) %>% 
                dplyr::mutate(status=paste0(status, "_emi")) %>%
                spread(status,emi))
  if (any(out$iso != out$iso2)) stop()  
  out <- out %>% 
    dplyr::select(-iso2)
  out[which(is.na(out), arr.ind = TRUE)] = 0
  
  return(out)
}
merge_allData     <- function(iCountry, iData) {
  out <- left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              left_join(
                left_join(iCountry,                      # SSA countries (name + iso)
                          iData$coal,  by=c("country")), # coal data
                iData$oil,   by=c("iso")),     # oil data
              iData$gas ,  by=c("iso")),     # gas data
            iData$bio,   by=c("iso")),     # bio data
          iData$ren ,  by=c("iso")),     # ren data
        iData$geo ,  by=c("iso")),     # geo data
      iData$hydro, by=c("iso")),     # hydro data
    iData$nuc,   by=c("iso"))      # nuc data
  
  # Replace NA values by 0
  out[which(is.na(out), arr.ind = TRUE)] = 0
  
  return(out)
}
postprocess_data  <- function(iData) {
  # Set factors for fuels, status, variables and units
  fuels  <- c("coal"="coal", "oil"="oil", "natgas"="gas", "biowas"="bio", "nuclear"="nuc", "hydro"="hydro", "geo"="geo", "solwin"="ren")
  status <- c("construction"="C", "operating"="O", "pre-construction"="P", "shelved"="S", "retired"="R")
  vars   <- c("PE power plant"="pe", "CO2 power plant"="emi")
  units  <- c("ktoe/yr", "MtCO2/yr")
  
  lvls <- unlist(lapply(unlist(lapply(paste(fuels), function(x) paste0(x, "_", paste(status)))), function(x) paste0(x, "_", paste(vars))))
  labs <- unlist(lapply(unlist(lapply(names(fuels), function(x) paste0(x, " (", names(status), ")"))), function(x) paste0(names(vars), " - ", x)))
  
  out <- data.frame(variable=lvls) %>% 
    left_join(gather(iData, variable, value) %>% 
                mutate(fuel   = sapply(variable, function(x) strsplit(x, "_")[[1]][1])) %>% 
                mutate(status = sapply(variable, function(x) strsplit(x, "_")[[1]][2])) %>%
                mutate(var    = sapply(variable, function(x) strsplit(x, "_")[[1]][3])), 
              by = c("variable")) %>% 
    mutate(value = ifelse(is.na(value), 0, value))
  
  # Apply factors to data
  out$variable <- factor(out$variable, 
                         levels=lvls, 
                         labels=labs)
  out <- out %>%
    select(-fuel, -status,- var) %>% 
    spread(variable, value)
  
  # Add future year and population and GDP data 
  # (pop and gdp data are abitrarily set to 1 now
  # but are overwritten later after the extrapolation process)
  out$year <- u_year
  
  out$`GDP - MER`             <- 1
  out$`GDP - PPP`             <- 1
  out$`Population [million]`  <- 1
  
  return(out)
}
format_data <- function(iData) {
  out <- data.frame(
    "year"                               = iData$Time, 
    "CO2 total - coal"                   = sum(c(iData$`Emi|Coal|Construction [MtCO2/yr]`,iData$`Emi|Coal|Operating [MtCO2/yr]`,iData$`Emi|Coal|Pre-Construction [MtCO2/yr]`,iData$`Emi|Coal|Shelved [MtCO2/yr]`, na.rm=TRUE)), # - iData$`Emi|Coal|Retired [MtCO2/yr]`,
    "CO2 Coal - Pre-Construction"        = iData$`Emi|Coal|Pre-Construction [MtCO2/yr]`,
    "CO2 Coal - Construction"            = iData$`Emi|Coal|Construction [MtCO2/yr]`,
    "CO2 Coal - Operating"               = iData$`Emi|Coal|Operating [MtCO2/yr]`,
    "CO2 Coal - Shelved"                 = iData$`Emi|Coal|Shelved [MtCO2/yr]`,
    "CO2 Coal - Non-Power Activities"    = iData$`Emi|Coal|Non-Power Activities [MtCO2/yr]`,
    "CO2 Oil "                           = sum(c(iData$`Emi|Oil|Construction [MtCO2/yr]`,iData$`Emi|Oil|Operating [MtCO2/yr]`,iData$`Emi|Oil|Pre-Construction [MtCO2/yr]`,iData$`Emi|Oil|Shelved [MtCO2/yr]`, na.rm=TRUE)), # - iData$`Emi|Oil|Retired [MtCO2/yr]`,
    "CO2 Natural Gas "                   = sum(c(iData$`Emi|Gas|Construction [MtCO2/yr]`,iData$`Emi|Gas|Operating [MtCO2/yr]`,iData$`Emi|Gas|Pre-Construction [MtCO2/yr]`,iData$`Emi|Gas|Shelved [MtCO2/yr]`, na.rm=TRUE)), # - iData$`Emi|Gas|Retired [MtCO2/yr]`,
    "CO2 Other "                         = sum(c(iData$`Emi|Biomass|Construction [MtCO2/yr]`,iData$`Emi|Biomass|Operating [MtCO2/yr]`,iData$`Emi|Biomass|Pre-Construction [MtCO2/yr]`,iData$`Emi|Biomass|Shelved [MtCO2/yr]`, na.rm=TRUE)), # - iData$`Emi|Biomass|Retired [MtCO2/yr]`
    "CO2 Sectoral Approach (Mt of CO2)"  = 0,	
    "Coal and Coal Products"             = 0,
    "Coal - Pre-Construction"            = 0,
    "Coal - Construction"                = 0,
    "Coal - Operating"                   = 0,
    "Coal - Shelved"                     = 0,
    "Coal - Non-Power Activities"        = 0,
    "Oil "                               = 0, 	
    "Natural Gas "                       = 0, 	
    "Other "                             = 0, 	
    "CO2 Reference Approach (Mt of CO2)" = 0,	
    "Total Primary Energy Supply (PJ)"   = 0,	
    "Total Primary Energy Supply (Mtoe)" = 0,	
    "GDP (billion 2000 US$ using exchange rates)" = iData$`GDP - MER`,	
    "GDP (billion 2000 US$ using PPPs)"           = iData$`GDP - PPP`,	
    "Population (millions)"                       = iData$`Population [million]`,
    "PE Coal and Coal Products"                   = sum(c(iData$`PE|Coal|Construction [EJ/yr]`, iData$`PE|Coal|Operating [EJ/yr]`, iData$`PE|Coal|Pre-Construction [EJ/yr]`, iData$`PE|Coal|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Coal|Retired [EJ/yr]`,
    "PE Coal - Pre-Construction"                  = iData$`PE|Coal|Pre-Construction [EJ/yr]`,
    "PE Coal - Construction"                      = iData$`PE|Coal|Construction [EJ/yr]`,
    "PE Coal - Operating"                         = iData$`PE|Coal|Operating [EJ/yr]`,
    "PE Coal - Shelved"                           = iData$`PE|Coal|Shelved [EJ/yr]`,
    "PE Coal - Non-Power Activities"              = iData$`PE|Coal|Non-Power Activities [EJ/yr]`,
    "PE Peat"                                     = 0,	
    "PE Crude, NGL and Feedstocks"                = sum(c(iData$`PE|Oil|Construction [EJ/yr]`, iData$`PE|Oil|Operating [EJ/yr]`, iData$`PE|Oil|Pre-Construction [EJ/yr]`, iData$`PE|Oil|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Oil|Retired [EJ/yr]`,
    "PE Petroleum Products"                       = 0,	
    "PE Natural Gas"                              = sum(c(iData$`PE|Gas|Construction [EJ/yr]`, iData$`PE|Gas|Operating [EJ/yr]`, iData$`PE|Gas|Pre-Construction [EJ/yr]`, iData$`PE|Gas|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Gas|Retired [EJ/yr]`,
    "PE Nuclear"                                  = sum(c(iData$`PE|Nuclear|Construction [EJ/yr]`, iData$`PE|Nuclear|Operating [EJ/yr]`, iData$`PE|Nuclear|Pre-Construction [EJ/yr]`, iData$`PE|Nuclear|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Nuclear|Retired [EJ/yr]`,
    "PE Hydro"                                    = sum(c(iData$`PE|Hydro|Construction [EJ/yr]`, iData$`PE|Hydro|Operating [EJ/yr]`, iData$`PE|Hydro|Pre-Construction [EJ/yr]`, iData$`PE|Hydro|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Hydro|Retired [EJ/yr]`,
    "PE Geothermal"                               = sum(c(iData$`PE|Geo|Construction [EJ/yr]`, iData$`PE|Geo|Operating [EJ/yr]`, iData$`PE|Geo|Pre-Construction [EJ/yr]`, iData$`PE|Geo|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Geo|Retired [EJ/yr]`,
    "PE Solar/Wind/Other"                         = sum(c(iData$`PE|Renewables|Construction [EJ/yr]`, iData$`PE|Renewables|Operating [EJ/yr]`, iData$`PE|Renewables|Pre-Construction [EJ/yr]`, iData$`PE|Renewables|Shelved [EJ/yr]`), na.rm=TRUE), #- iData$`PE|Renewables|Retired [EJ/yr]`,
    "PE Combustible Renewables and Waste"         = sum(c(iData$`PE|Biomass|Construction [EJ/yr]`, iData$`PE|Biomass|Operating [EJ/yr]`, iData$`PE|Biomass|Pre-Construction [EJ/yr]`, iData$`PE|Biomass|Shelved [EJ/yr]`), na.rm=TRUE), # - iData$`PE|Biomass|Retired [EJ/yr]`,
    "PE Heat Production from non-specified comb.fuels"     = 0,
    "PE Electricity"                                       = 0,
    "Heat"                                                 = 0,
    "Total Primary Energy Supply (ktoe)"                   = 0
  ) 
  out_names = c(
    "year",
    "CO2 Coal and Coal Products", "CO2 Coal - Pre-Construction", "CO2 Coal - Construction", "CO2 Coal - Operating", "CO2 Coal - Shelved", "CO2 Coal - Non-Power Activities",
    "CO2 Oil ",
    "CO2 Natural Gas ", 
    "CO2 Other ", 
    "CO2 Sectoral Approach (Mt of CO2)",
    "Coal and Coal Products", "Coal - Pre-Construction", "Coal - Construction", "Coal - Operating", "Coal - Shelved", "Coal - Non-Power Activities",
    "Oil ", "Natural Gas ", "Other ", "CO2 Reference Approach (Mt of CO2)",
    "Total Primary Energy Supply (PJ)","Total Primary Energy Supply (Mtoe)",
    "GDP (billion 2000 US$ using exchange rates)","GDP (billion 2000 US$ using PPPs)",
    "Population (millions)",
    "PE Coal and Coal Products", "PE Coal - Pre-Construction", "PE Coal - Construction", "PE Coal - Operating", "PE Coal - Shelved", "PE Coal - Non-Power Activities", 
    "PE Peat",
    "PE Crude, NGL and Feedstocks", 
    "PE Petroleum Products",
    "PE Natural Gas", 
    "PE Nuclear", 
    "PE Hydro", 
    "PE Geothermal",
    "PE Solar/Wind/Other",
    "PE Combustible Renewables and Waste",
    "PE Heat Production from non-specified comb.fuels","PE Electricity","Heat","Total Primary Energy Supply (ktoe)")  
  if (length(out) != length(out_names)) stop("Lengths are different!")
  names(out) <- out_names
  
  return(out)
}
plot_extrapolation <- function(iData) {
  
  out <- iData
  
  # Initialise extrapolation
  fuels <- c("peat", "coal", "oil", "natgas", "nuclear", "hydro", "geo", 
             "solwin", "biowas", "elec", "heat")
  variables <- c(
    paste0("PE other - ", fuels),
    "PE power plant - elec", "PE power plant - heat", "PE power plant - peat",
    "POP",
    #"GDP MER",
    "GDP PPP",
    "TPES", "TFC")
  periods <- list(
    "PE other - peat" = 2000:2015,
    "PE other - coal" = 2010:2015,
    "PE other - oil" = 2010:2015,
    "PE other - netgas" = 2003:2015,
    "PE other - nuclear" = 2000:2015,
    "PE other - hydro" = 2006:2015,
    "PE other - geo" = 2010:2015,
    "PE other - solwin" = 2010:2015,
    "PE other - biowas" = 2010:2015,
    "PE other - elec" = 2000:2015,
    "PE other - heat" = 2006:2015,
    "PE power plant - elec" = 2000:2015,
    "PE power plant - heat" = 2000:2015,
    "PE power plant - peat" = 2000:2015,
    "POP" = 2010:2015,
    #"GDP MER" = 2005:2015,
    "GDP PPP" = 2005:2015,
    "TPES" = 2005:2015,
    "TFC" = 2005:2015
  )
  
  #lapply(1:length(variables), function(x) rep(2010:2015))
  names(periods)  <- variables
  
  svglite::svglite(file="../plots/FigureS2.svg", width = 12, height = 18)
  
  par(mfrow=c(6,3), las=1, cex=1)
  
  # Extrapolate other activity data (i.e. not power plant) and population and GDP data    
  for (var in variables) {
    xdata <- periods[[var]]
    ydata <- as.numeric((out[which(out$year %in% periods[[var]]), var][[var]]))
    res   <- lm(y~x, data = data.frame(x=periods[[var]],y=ydata))
    
    plot(1990:2015, as.numeric((out[which(out$year %in% 1990:2015), var][[var]])), 
         main=var, xlab="", ylab="Primary energy [ktoe]")
    points(periods[[var]], ydata, pch=21, col="red", bg="red")
    lines(periods[[var]], as.numeric(res$coefficients[1] + res$coefficients[2]*periods[[var]]), col="red")
    
    #out[which(out$year == u_year), var] <- as.numeric(res$coefficients[1] + res$coefficients[2]*u_year)
  }
  
  par(mfrow=c(1,1))
  
  dev.off()
}
adjust_data <- function(iData, iEF, iRatio_cpc, iRatio_cs) {
  
  out <- iData
  
  # Adjust success ratios of pre-construction and shelved categories
  out$`PE power plant - coal (pre-construction)`    <- out$`PE power plant - coal (pre-construction)`*iRatio_cpc
  out$`PE power plant - coal (shelved)`             <- out$`PE power plant - coal (shelved)`*iRatio_cs
  out$`CO2 power plant - coal (pre-construction)`   <- out$`CO2 power plant - coal (pre-construction)`*iRatio_cpc
  out$`CO2 power plant - coal (shelved)`            <- out$`CO2 power plant - coal (shelved)`*iRatio_cs
  
  # Initialise extrapolation
  fuels <- c("peat", "coal", "oil", "natgas", "nuclear", "hydro", "geo", 
             "solwin", "biowas", "elec", "heat")
  variables <- c(
    paste0("PE other - ", fuels),
    "PE power plant - elec", "PE power plant - heat", "PE power plant - peat",
    "POP",
    "GDP MER",
    "GDP PPP",
    "TPES", "TFC")
  periods <- list(
    "PE other - peat" = 2000:2015,
    "PE other - coal" = 2010:2015,
    "PE other - oil" = 2010:2015,
    "PE other - netgas" = 2003:2015,
    "PE other - nuclear" = 2000:2015,
    "PE other - hydro" = 2006:2015,
    "PE other - geo" = 2010:2015,
    "PE other - solwin" = 2010:2015,
    "PE other - biowas" = 2010:2015,
    "PE other - elec" = 2000:2015,
    "PE other - heat" = 2006:2015,
    "PE power plant - elec" = 2000:2015,
    "PE power plant - heat" = 2000:2015,
    "PE power plant - peat" = 2000:2015,
    "POP" = 2010:2015,
    "GDP MER" = 2005:2015,
    "GDP PPP" = 2005:2015,
    "TPES" = 2005:2015,
    "TFC" = 2005:2015
  )
  
  #lapply(1:length(variables), function(x) rep(2010:2015))
  names(periods)  <- variables
  
  # Extrapolate other activity data (i.e. not power plant) and population and GDP data    
  for (var in variables) {
    xdata <- periods[[var]]
    ydata <- as.numeric((out[which(out$year %in% periods[[var]]), var][[var]]))
    res   <- lm(y~x, data = data.frame(x=periods[[var]],y=ydata))
    
    #plot(1990:2015, as.numeric((out[which(out$year %in% 1990:2015), var][[var]])), main=var)
    #points(periods[[var]], ydata, pch=21, col="red", bg="red")
    #lines(periods[[var]], as.numeric(res$coefficients[1] + res$coefficients[2]*periods[[var]]), col="red")
    
    out[which(out$year == u_year), var] <- as.numeric(res$coefficients[1] + res$coefficients[2]*u_year)
  }
  
  # Compute associated CO2 emissions
  out$`CO2 other - coal`[which(out$year == u_year)]   <- iEF$coal * out$`PE other - coal`[which(out$year == u_year)]
  out$`CO2 other - oil`[which(out$year == u_year)]    <- iEF$oil  * out$`PE other - oil`[which(out$year == u_year)]
  out$`CO2 other - natgas`[which(out$year == u_year)] <- iEF$gas  * out$`PE other - natgas`[which(out$year == u_year)]
  out$`CO2 other - total`[which(out$year == u_year)]  <-  out$`CO2 other - coal`[which(out$year == u_year)] + out$`CO2 other - oil`[which(out$year == u_year)] + out$`CO2 other - natgas`[which(out$year == u_year)]
  out$`CO2 power plant - total`[which(out$year == u_year)]  <-  out$`CO2 power plant - coal (operating)`[which(out$year == u_year)] + 
    out$`CO2 power plant - coal (construction)`[which(out$year == u_year)] + 
    out$`CO2 power plant - coal (pre-construction)`[which(out$year == u_year)] + 
    out$`CO2 power plant - coal (shelved)`[which(out$year == u_year)] + 
    out$`CO2 power plant - oil`[which(out$year == u_year)] + out$`CO2 power plant - natgas`[which(out$year == u_year)]
  
  # Recompute PE and CO2 totals
  # Coal
  out$`PE total - coal`[which(out$year == u_year)] <- 
    out$`PE power plant - coal (operating)`[which(out$year == u_year)] + 
    out$`PE power plant - coal (construction)`[which(out$year == u_year)] + 
    out$`PE power plant - coal (pre-construction)`[which(out$year == u_year)] + 
    out$`PE power plant - coal (shelved)`[which(out$year == u_year)] + 
    out$`PE other - coal`[which(out$year == u_year)] 
  out$`CO2 total - coal`[which(out$year == u_year)] <- 
    out$`CO2 power plant - coal (operating)`[which(out$year == u_year)] + 
    out$`CO2 power plant - coal (construction)`[which(out$year == u_year)] + 
    out$`CO2 power plant - coal (pre-construction)`[which(out$year == u_year)] + 
    out$`CO2 power plant - coal (shelved)`[which(out$year == u_year)] + 
    out$`CO2 other - coal`[which(out$year == u_year)]
  
  # By fuel
  for (k_fuel in fuels[which(fuels != "coal")]) {
    out[which(out$year == u_year), paste0("PE total - ", k_fuel)] <- out[which(out$year == u_year), paste0("PE power plant - ", k_fuel)] + 
      out[which(out$year == u_year), paste0("PE other - ", k_fuel)]
    if (k_fuel %in% c("oil", "natgas")) {
      out[which(out$year == u_year), paste0("CO2 total - ", k_fuel)] <- out[which(out$year == u_year), paste0("CO2 power plant - ", k_fuel)] + 
        out[which(out$year == u_year), paste0("CO2 other - ", k_fuel)]
    }
  }
  
  # Overall
  out$`PE power plant - total`[which(out$year == u_year)]  <- sum(out[which(out$year == u_year), paste0("PE power plant - ", fuels[which(fuels != "coal")])]) + 
    out$`PE power plant - coal (operating)`[which(out$year == u_year)] + 
    out$`PE power plant - coal (construction)`[which(out$year == u_year)] + 
    out$`PE power plant - coal (pre-construction)`[which(out$year == u_year)] + 
    out$`PE power plant - coal (shelved)`[which(out$year == u_year)]
  out$`PE other - total`[which(out$year == u_year)]  <- sum(out[which(out$year == u_year), paste0("PE other - ", fuels)])
  out$`PE total - total`[which(out$year == u_year)]  <- sum(out[which(out$year == u_year), paste0("PE total - ", fuels)])
  out$`CO2 total - total`[which(out$year == u_year)] <- sum(out[which(out$year == u_year), paste0("CO2 total - ", c("coal", "oil", "natgas"))])
  
  # Other
  out$`CO2 total - other`[which(out$year == u_year)] <- 0.0
  
  return(out)
  
}
plot_kayaDecompositionFFext2_5year  <- function(iData, MODE=NULL, i_ylim=c(-50,200), iName=NULL) {
  
  #== Initialization ===========================================================
  # Define Lapeyres function
  laspeyeres <- function(v1,v2,v3,v4,dv1,dv2,dv3,dv4) {
    
    out <- dv1*v2*v3*v4 +
      1/2*dv1*(dv2* v3* v4 +  v2*dv3* v4 + v2* v3*dv4) +
      1/3*dv1*(dv2*dv3* v4 + dv2* v3*dv4 + v2*dv3*dv4) +
      1/4*dv1*(dv2*dv3*dv4)
    
    return(out)
  }
  
  #== Process data =============================================================
  cat("Processing data...\n")
  iData = iData %>%
    dplyr::select(country,year,`POP`, `GDP PPP`,
                  `TPES`, 
                  `CO2 total - total`, `PE total - total`,
                  `CO2 total - coal`,`CO2 other - coal`, `CO2 power plant - coal (operating)`,`CO2 power plant - coal (construction)`, `CO2 power plant - coal (pre-construction)`, `CO2 power plant - coal (shelved)`,
                  `CO2 total - oil`, `CO2 total - natgas`, `CO2 total - other`,
                  `PE total - coal`, `PE other - coal`, `PE power plant - coal (operating)`, `PE power plant - coal (construction)`, `PE power plant - coal (pre-construction)`, `PE power plant - coal (shelved)`, 
                  `PE total - oil`, `PE total - natgas`, `PE total - peat`, `PE total - nuclear`, 
                  `PE total - hydro`, `PE total - geo`, `PE total - solwin`, `PE total - heat`, `PE total - biowas`,
                  `PE total - elec`) %>% 
    dplyr::mutate(`affluence`        = `GDP PPP`           / `POP`) %>% 
    dplyr::mutate(`energy intensity` = `PE total - total`  / `GDP PPP`) %>% 
    dplyr::mutate(`carbon intensity` = `CO2 total - total` / `PE total - total`) %>% 
    dplyr::mutate(`POP`              = `POP`) %>% 
    dplyr::rename(p = `POP`) %>% 
    dplyr::rename(a = `affluence`) %>% 
    dplyr::rename(e = `energy intensity`) %>% 
    dplyr::rename(k = `carbon intensity`) %>% 
    dplyr::rename(F = `CO2 total - total`) %>% 
    dplyr::rename(emi_coal    = `CO2 total - coal`) %>% 
    dplyr::rename(emi_coal_o  = `CO2 power plant - coal (operating)`) %>%
    dplyr::rename(emi_coal_c  = `CO2 power plant - coal (construction)`) %>%
    dplyr::rename(emi_coal_pc = `CO2 power plant - coal (pre-construction)`) %>%
    dplyr::rename(emi_coal_s  = `CO2 power plant - coal (shelved)`) %>%
    dplyr::rename(emi_coal_z  = `CO2 other - coal`) %>%
    dplyr::rename(emi_oil     = `CO2 total - oil`) %>% 
    dplyr::rename(emi_gas     = `CO2 total - natgas`) %>% 
    dplyr::mutate(emi_other   = `CO2 total - other`) %>% 
    dplyr::mutate(pe_coal     = `PE total - coal`) %>% 
    dplyr::mutate(pe_coal_o   = `PE power plant - coal (operating)`) %>%
    dplyr::mutate(pe_coal_c   = `PE power plant - coal (construction)`) %>%
    dplyr::mutate(pe_coal_pc  = `PE power plant - coal (pre-construction)`) %>%
    dplyr::mutate(pe_coal_s   = `PE power plant - coal (shelved)`) %>%
    dplyr::mutate(pe_coal_z   = `PE other - coal`) %>%
    dplyr::mutate(pe_oil      = `PE total - oil`) %>% 
    dplyr::mutate(pe_gas      = `PE total - natgas`) %>%
    dplyr::mutate(pe_other    = (`PE total - peat`+`PE total - hydro` + `PE total - nuclear` + `PE total - geo` + `PE total - solwin` + `PE total - biowas` + 
                                   `PE total - heat`+`PE total - elec`)) %>% 
    dplyr::mutate(k_coal    = emi_coal     /pe_coal) %>% 
    dplyr::mutate(k_coal_o  = emi_coal_o   /pe_coal_o) %>% 
    dplyr::mutate(k_coal_c  = emi_coal_c   /pe_coal_c) %>% 
    dplyr::mutate(k_coal_pc = emi_coal_pc  /pe_coal_pc) %>% 
    dplyr::mutate(k_coal_s  = emi_coal_s   /pe_coal_s) %>% 
    dplyr::mutate(k_coal_z  = emi_coal_z   /pe_coal_z) %>% 
    dplyr::mutate(k_oil     = emi_oil      /pe_oil) %>% 
    dplyr::mutate(k_gas     = emi_gas      /pe_gas) %>% 
    dplyr::mutate(k_other   = emi_other    /pe_other) %>% 
    dplyr::mutate(pe        = `PE total - total`) %>%
    dplyr::select(country,year,p,a,e,k,F,
                  emi_coal,emi_coal_o,emi_coal_c,emi_coal_pc,emi_coal_s,emi_coal_z,emi_oil,emi_gas,emi_other,
                  pe,pe_coal,pe_coal_o,pe_coal_c,pe_coal_pc,pe_coal_s,pe_coal_z,pe_oil,pe_gas,pe_other,
                  k_coal,k_coal_o,k_coal_c,k_coal_pc,k_coal_s,k_coal_z,k_oil,k_gas,k_other) %>% 
    dplyr::filter(year %in% seq(1985,2025,5))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(country) %>%
    arrange(year) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpecoal_o   = (pe_coal_o   -0)) %>%
    mutate(Dpecoal_c   = (pe_coal_c   -0)) %>%
    mutate(Dpecoal_pc  = (pe_coal_pc -0)) %>%
    mutate(Dpecoal_s   = (pe_coal_s   -0)) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    # mutate(Dkc_a   = 1/pe *(k_coal_a   *pe_coal_a   - lag(k_coal_a)   *lag(pe_coal_a)   - Dpecoal_a   *lag(k))) %>% 
    # mutate(Dkc_o   = 1/pe *(k_coal_o   *pe_coal_o   - lag(k_coal_o)   *lag(pe_coal_o)   - Dpecoal_o   *lag(k))) %>% 
    # mutate(Dkc_p   = 1/pe *(k_coal_p   *pe_coal_p   - lag(k_coal_p)   *lag(pe_coal_p)   - Dpecoal_p   *lag(k))) %>% 
    # mutate(Dkc_ppd = 1/pe *(k_coal_ppd *pe_coal_ppd - lag(k_coal_ppd) *lag(pe_coal_ppd) - Dpecoal_ppd *lag(k))) %>% 
    # mutate(Dkc_s   = 1/pe *(k_coal_s   *pe_coal_s   - lag(k_coal_s)   *lag(pe_coal_s)   - Dpecoal_s   *lag(k))) %>%
    mutate(Dkc_o   = 1/pe *(k_coal_o   *pe_coal_o   -0 - Dpecoal_o   *lag(k))) %>% 
    mutate(Dkc_c   = 1/pe *(k_coal_c   *pe_coal_c   -0 - Dpecoal_c   *lag(k))) %>% 
    mutate(Dkc_pc  = 1/pe *(k_coal_pc  *pe_coal_pc  -0 - Dpecoal_pc  *lag(k))) %>% 
    mutate(Dkc_s   = 1/pe *(k_coal_s   *pe_coal_s   -0 - Dpecoal_s   *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R  = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De)
  
  print(iData %>% 
          mutate(check  = 100*(1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De)/
                   (lag(p)*lag(a)*lag(e) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De)) %>% 
          select(country,year,check))  
  
  iData = iData %>% 
    mutate(Cp = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    dplyr::mutate(Ckc        = Dkc*R) 
  if (is.null(MODE)) {
    #print("original")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Dkc_o*R)) %>%
      dplyr::mutate(Ckc_c      = Dkc_c*R) %>%
      dplyr::mutate(Ckc_pc     = Dkc_pc*R) %>%
      dplyr::mutate(Ckc_s      = Dkc_s*R) %>% 
      dplyr::mutate(Ckc_z      = Dkc_z*R)
  } else {
    #print("share")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Ckc*pe_coal_o/pe_coal)) %>% 
      dplyr::mutate(Ckc_c      = Ckc*pe_coal_c/pe_coal) %>% 
      dplyr::mutate(Ckc_pc     = Ckc*pe_coal_pc/pe_coal) %>% 
      dplyr::mutate(Ckc_s      = Ckc*pe_coal_s/pe_coal) %>% 
      dplyr::mutate(Ckc_z      = Ckc*pe_coal_z/pe_coal)
  }
  
  iData <- iData %>% 
    mutate(Cko = Dko*R) %>% 
    mutate(Ckg = Dkg*R) %>% 
    mutate(Ckr = Dkr*R) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) %>% 
    filter(year != 1985) %>% 
    mutate(period = factor(year, levels = c(seq(1990,2015,5), 2025), labels=c("1985-1990",
                                                                              "1990-1995",
                                                                              "1995-2000",
                                                                              "2000-2005",
                                                                              "2005-2010",
                                                                              "2010-2015",
                                                                              "2015-2025"), ordered=TRUE))
  
  print(iData %>% filter(variable %in% c("DF", "Cp","Ca","Ce","Ck","Ckc","Ckc_z", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s","Cko","Ckg","Ckr","R")) %>% spread(variable,value)) #,"Ckc_s"
  iData_all = iData
  
  iData = iData %>% mutate(variable = factor(variable, c(rev(c("Ckc","Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s")), "Cko", "Ckg", "Ckr"), ordered=TRUE)) #,"Ckc_s"
  
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(8, "Set1")
  red_shades <- colorRampPalette(5,colors=c("#8C001A", "#E77471"))
  red_shades <- colorRampPalette(5,colors=c("#7E3517", "#E2A76F"))
  
  cols[2:6] <- red_shades(5)     # Coal Status
  cols[1]   <- cols[2]
  cols[7]   <- "black"           # Crude oil 
  cols[8]   <- "#ff7256"         # Natural gas "#0020C2" 
  cols[9]   <- "#0000ff"         # Other  "#008000"
  
  names(cols)  = c("Ckc","Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")
  colour_scale = scale_fill_manual(
    name   = " ",
    values = cols,
    breaks = c("Ckc", "Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr"),
    labels = c("Coal", "Coal (Non-MPA)", "Coal (Operating)", "Coal (Construction)", 
               "Coal (Pre-Construction)", "Coal (Shelved)", 
               "Crude Oil", "Natural Gas", "Other"))
  
  #-- Plot ---------------------------------------------------------------------
  # p = ggplot()
  # p = p +
  #   geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("1995-2005","2005-2015", "2015-2025"), variable %in% c("Ckc_o", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
  #   geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("1995-2005","2005-2015", "2015-2025"), variable %in% c("Ckc_o", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack")# +
  # # geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("2013-2018"), variable %in% c("Ckc_a","Ckc_o","Ckc_p","Ckc_ppd","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
  # # geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("2013-2018"), variable %in% c("Ckc_a","Ckc_o","Ckc_p","Ckc_ppd","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack")# +
  # #geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1988-1993", variable == "DF"))
  # 
  # # Formatting
  # p = p +
  #   colour_scale +
  #   ylim(i_ylim) +
  #   ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
  #   theme_bw() +
  #   theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  # print(p)
  
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("1990-1995","1995-2000","2000-2005","2005-2010","2010-2015"), variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("1990-1995","1995-2000","2000-2005","2005-2010","2010-2015"), variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("2015-2025"), variable %in% c("Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("2015-2025"), variable %in% c("Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    #geom_segment(aes(x=period-0.5, xend=period+0.5, y=value, yend=value), data=iData_all %>% filter(period != "1985-1995", variable == "Ck"), color="black", size=2) +
    geom_point(aes(x=period, y=value), data=iData_all %>% filter(period != "1985-1990", variable == "Ck"), color="black", fill="white", size=2, pch=21)
  
  # Formatting
  p = p +
    colour_scale +
    ylim(i_ylim) +
    ylab(expression("Change in "*CO[2]*" emissions [Mt "*CO[2]*"]")) + xlab("") +
    #ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    guides(fill=guide_legend(nrow=2,bycol=TRUE)) +
    theme(legend.position="bottom", legend.title=element_text(size=6), legend.text=element_text(size=6), 
          axis.text=element_text(size=8), axis.title=element_text(size=8)) #, axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  #ggsave(p, filename = paste0("CO2EmiChange_SSA_10year_", format(Sys.time(), "%Y-%m-%d_%H"), ".png"), dpi = 1200, height = 10, width = 10, units = "cm")
  if (!is.null(iName)) ggsave(p, filename = paste0("CO2EmiChange_SSA_5year_sensitivity_", iName, "_", format(Sys.time(), "%Y-%m-%d_%H"), ".svg"), dpi = 1200, height = 13, width = 10, units = "cm")
  
  return(list(data=iData_all, plot=p))
}
plot_kayaDecompositionFFext2_10year <- function(iData, MODE=NULL, i_ylim=c(-50,200), iName=NULL) {
  
  #== Initialization ===========================================================
  # Define Lapeyres function
  laspeyeres <- function(v1,v2,v3,v4,dv1,dv2,dv3,dv4) {
    
    out <- dv1*v2*v3*v4 +
      1/2*dv1*(dv2* v3* v4 +  v2*dv3* v4 + v2* v3*dv4) +
      1/3*dv1*(dv2*dv3* v4 + dv2* v3*dv4 + v2*dv3*dv4) +
      1/4*dv1*(dv2*dv3*dv4)
    
    return(out)
  }
  
  #== Process data =============================================================
  #cat("Processing data...\n")
  iData <- iData %>%
    dplyr::select(country,year,`POP`, `GDP PPP`,
                  `TPES`, 
                  `CO2 total - total`, `PE total - total`,
                  `CO2 total - coal`,`CO2 other - coal`, `CO2 power plant - coal (operating)`,`CO2 power plant - coal (construction)`, `CO2 power plant - coal (pre-construction)`, `CO2 power plant - coal (shelved)`,
                  `CO2 total - oil`, `CO2 total - natgas`, `CO2 total - other`,
                  `PE total - coal`, `PE other - coal`, `PE power plant - coal (operating)`, `PE power plant - coal (construction)`, `PE power plant - coal (pre-construction)`, `PE power plant - coal (shelved)`, 
                  `PE total - oil`, `PE total - natgas`, `PE total - peat`, `PE total - nuclear`, 
                  `PE total - hydro`, `PE total - geo`, `PE total - solwin`, `PE total - heat`, `PE total - biowas`,
                  `PE total - elec`) %>% 
    dplyr::mutate(`affluence`        = `GDP PPP`           / `POP`) %>% 
    dplyr::mutate(`energy intensity` = `PE total - total`  / `GDP PPP`) %>% 
    dplyr::mutate(`carbon intensity` = `CO2 total - total` / `PE total - total`) %>% 
    dplyr::mutate(`POP`              = `POP`) %>% 
    dplyr::rename(p = `POP`) %>% 
    dplyr::rename(a = `affluence`) %>% 
    dplyr::rename(e = `energy intensity`) %>% 
    dplyr::rename(k = `carbon intensity`) %>% 
    dplyr::rename(F = `CO2 total - total`) %>% 
    dplyr::rename(emi_coal    = `CO2 total - coal`) %>% 
    dplyr::rename(emi_coal_o  = `CO2 power plant - coal (operating)`) %>%
    dplyr::rename(emi_coal_c  = `CO2 power plant - coal (construction)`) %>%
    dplyr::rename(emi_coal_pc = `CO2 power plant - coal (pre-construction)`) %>%
    dplyr::rename(emi_coal_s  = `CO2 power plant - coal (shelved)`) %>%
    dplyr::rename(emi_coal_z  = `CO2 other - coal`) %>%
    dplyr::rename(emi_oil     = `CO2 total - oil`) %>% 
    dplyr::rename(emi_gas     = `CO2 total - natgas`) %>% 
    dplyr::mutate(emi_other   = `CO2 total - other`) %>% 
    dplyr::mutate(pe_coal     = `PE total - coal`) %>% 
    dplyr::mutate(pe_coal_o   = `PE power plant - coal (operating)`) %>%
    dplyr::mutate(pe_coal_c   = `PE power plant - coal (construction)`) %>%
    dplyr::mutate(pe_coal_pc  = `PE power plant - coal (pre-construction)`) %>%
    dplyr::mutate(pe_coal_s   = `PE power plant - coal (shelved)`) %>%
    dplyr::mutate(pe_coal_z   = `PE other - coal`) %>%
    dplyr::mutate(pe_oil      = `PE total - oil`) %>% 
    dplyr::mutate(pe_gas      = `PE total - natgas`) %>%
    dplyr::mutate(pe_other    = (`PE total - peat`+`PE total - hydro` + `PE total - nuclear` + `PE total - geo` + `PE total - solwin` + `PE total - biowas` + 
                                   `PE total - heat`+`PE total - elec`)) %>% 
    dplyr::mutate(k_coal    = emi_coal     /pe_coal) %>% 
    dplyr::mutate(k_coal_o  = emi_coal_o   /pe_coal_o) %>% 
    dplyr::mutate(k_coal_c  = emi_coal_c   /pe_coal_c) %>% 
    dplyr::mutate(k_coal_pc = emi_coal_pc  /pe_coal_pc) %>% 
    dplyr::mutate(k_coal_s  = emi_coal_s   /pe_coal_s) %>% 
    dplyr::mutate(k_coal_z  = emi_coal_z   /pe_coal_z) %>% 
    dplyr::mutate(k_oil     = emi_oil      /pe_oil) %>% 
    dplyr::mutate(k_gas     = emi_gas      /pe_gas) %>% 
    dplyr::mutate(k_other   = emi_other    /pe_other) %>% 
    dplyr::mutate(pe        = `PE total - total`) %>%
    dplyr::select(country,year,p,a,e,k,F,
                  emi_coal,emi_coal_o,emi_coal_c,emi_coal_pc,emi_coal_s,emi_coal_z,emi_oil,emi_gas,emi_other,
                  pe,pe_coal,pe_coal_o,pe_coal_c,pe_coal_pc,pe_coal_s,pe_coal_z,pe_oil,pe_gas,pe_other,
                  k_coal,k_coal_o,k_coal_c,k_coal_pc,k_coal_s,k_coal_z,k_oil,k_gas,k_other) %>% 
    dplyr::filter(year %in% c(1985,1995,2005,2015,2025))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  #cat("  > Computing Laspeyres decomposition...\n")
  iData <- iData %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(year) %>% 
    dplyr::mutate(Dp         = (p-lag(p))) %>%
    dplyr::mutate(Da         = (a-lag(a))) %>%
    dplyr::mutate(De         = (e-lag(e))) %>%
    dplyr::mutate(Dk         = (k-lag(k))) %>%
    dplyr::mutate(DF         = (F-lag(F))) %>%
    dplyr::mutate(Dpecoal    = (pe_coal -lag(pe_coal))) %>%
    dplyr::mutate(Dpecoal_o  = (pe_coal_o   -0)) %>%
    dplyr::mutate(Dpecoal_c  = (pe_coal_c   -0)) %>%
    dplyr::mutate(Dpecoal_pc = (pe_coal_pc  -0)) %>%
    dplyr::mutate(Dpecoal_s  = (pe_coal_s   -0)) %>%
    dplyr::mutate(Dpecoal_z  = (pe_coal_z   -0)) %>%
    dplyr::mutate(Dpeoil     = (pe_oil  -lag(pe_oil))) %>%
    dplyr::mutate(Dpegas     = (pe_gas  -lag(pe_gas))) %>%
    dplyr::mutate(Dpeother   = (pe_other-lag(pe_other))) %>%
    dplyr::mutate(Dkc        = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    dplyr::mutate(Dkc_o      = 1/pe *(k_coal_o   *pe_coal_o   -0 - Dpecoal_o   *lag(k))) %>% 
    dplyr::mutate(Dkc_c      = 1/pe *(k_coal_c   *pe_coal_c   -0 - Dpecoal_c   *lag(k))) %>% 
    dplyr::mutate(Dkc_pc     = 1/pe *(k_coal_pc  *pe_coal_pc  -0 - Dpecoal_pc  *lag(k))) %>% 
    dplyr::mutate(Dkc_s      = 1/pe *(k_coal_s   *pe_coal_s   -0 - Dpecoal_s   *lag(k))) %>% 
    dplyr::mutate(Dkc_z      = 1/pe *(k_coal_z   *pe_coal_z   -0 - Dpecoal_z   *lag(k))) %>% 
    dplyr::mutate(Dko        = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    dplyr::mutate(Dkg        = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    dplyr::mutate(Dkr        = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    dplyr::mutate(R          = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    dplyr::mutate(ratio      = (1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De)/R) %>% 
    dplyr::mutate(Cp         = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    dplyr::mutate(Ca         = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    dplyr::mutate(Ce         = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    dplyr::mutate(Ck         = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    dplyr::mutate(Ckc        = Dkc*R) 
  if (is.null(MODE)) {
    #print("original")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Dkc_o*R)) %>%
      dplyr::mutate(Ckc_c      = Dkc_c*R) %>%
      dplyr::mutate(Ckc_pc     = Dkc_pc*R) %>%
      dplyr::mutate(Ckc_s      = Dkc_s*R) %>% 
      dplyr::mutate(Ckc_z      = Dkc_z*R)
  } else {
    #print("share")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Ckc*pe_coal_o/pe_coal)) %>% 
      dplyr::mutate(Ckc_c      = Ckc*pe_coal_c/pe_coal) %>% 
      dplyr::mutate(Ckc_pc     = Ckc*pe_coal_pc/pe_coal) %>% 
      dplyr::mutate(Ckc_s      = Ckc*pe_coal_s/pe_coal) %>% 
      dplyr::mutate(Ckc_z      = Ckc*pe_coal_z/pe_coal)
  }
  
  iData <- iData %>% 
    dplyr::mutate(Cko        = Dko*R) %>% 
    dplyr::mutate(Ckg        = Dkg*R) %>% 
    dplyr::mutate(Ckr        = Dkr*R) %>%
    dplyr::ungroup() %>% 
    tidyr::gather(variable,value,-country,-year) %>% 
    dplyr::filter(year != 1985) %>% 
    dplyr::mutate(period = factor(year, levels = c(seq(1995,2025,10)), labels=c("1985-1995",
                                                                                "1995-2005",
                                                                                "2005-2015",
                                                                                "2015-2025"), ordered=TRUE))
  
  
  print(iData %>% filter(variable %in% c("DF", "Cp","Ca","Ce","Ck","Ckc", "Ckc_z", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s","Cko","Ckg","Ckr","R")) %>% spread(variable,value) %>% mutate(ratio=R/DF)) #,"Ckc_s"
  
  iData_all = iData
  
  iData = iData %>% mutate(variable = factor(variable, c(rev(c("Ckc","Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s")), "Cko", "Ckg", "Ckr"), ordered=TRUE)) #,"Ckc_s"
  
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(8, "Set1")
  red_shades <- colorRampPalette(5,colors=c("#8C001A", "#E77471"))
  red_shades <- colorRampPalette(5,colors=c("#7E3517", "#E2A76F"))
  
  cols[1:5] <- red_shades(5)     # Coal Status
  cols[6]   <- "black"           # Crude oil 
  cols[7]   <- "#ff7256"         # Natural gas "#0020C2" 
  cols[8]   <- "#0000ff"         # Other  "#008000"
  
  names(cols)  = c("Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")
  colour_scale = scale_fill_manual(
    name   = " ",
    values = cols,
    breaks = c("Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr"),
    labels = c("Coal (Non-MPA)", "Coal (Operating)", "Coal (Construction)", 
               "Coal (Pre-Construction)", "Coal (Shelved)", 
               "Crude Oil", "Natural Gas", "Other"))
  
  #-- Plot ---------------------------------------------------------------------
  # p = ggplot()
  # p = p +
  #   geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("1995-2005","2005-2015", "2015-2025"), variable %in% c("Ckc_o", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
  #   geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("1995-2005","2005-2015", "2015-2025"), variable %in% c("Ckc_o", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack")# +
  # # geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("2013-2018"), variable %in% c("Ckc_a","Ckc_o","Ckc_p","Ckc_ppd","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
  # # geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("2013-2018"), variable %in% c("Ckc_a","Ckc_o","Ckc_p","Ckc_ppd","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack")# +
  # #geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1988-1993", variable == "DF"))
  # 
  # # Formatting
  # p = p +
  #   colour_scale +
  #   ylim(i_ylim) +
  #   ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
  #   theme_bw() +
  #   theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  # print(p)
  
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("1995-2005","2005-2015"), variable %in% c("Ckc_z", "Ckc_o", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("1995-2005","2005-2015"), variable %in% c("Ckc_z", "Ckc_o", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period %in% c("2015-2025"), variable %in% c("Ckc_z", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period %in% c("2015-2025"), variable %in% c("Ckc_z", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    #geom_segment(aes(x=period-0.5, xend=period+0.5, y=value, yend=value), data=iData_all %>% filter(period != "1985-1995", variable == "Ck"), color="black", size=2) +
    geom_point(aes(x=period, y=value), data=iData_all %>% filter(period != "1985-1995", variable == "Ck"), color="black", fill="white", size=2, pch=21)
  
  # Formatting
  p = p +
    colour_scale +
    ylim(i_ylim) +
    ylab(expression("Change in "*CO[2]*" emissions [Mt "*CO[2]*"]")) + xlab("") +
    #ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    guides(fill=guide_legend(nrow=2,bycol=TRUE)) +
    theme(legend.position="bottom", legend.title=element_text(size=6), legend.text=element_text(size=6), 
          axis.text=element_text(size=8), axis.title=element_text(size=8)) #, axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  #ggsave(p, filename = paste0("CO2EmiChange_SSA_10year_", format(Sys.time(), "%Y-%m-%d_%H"), ".png"), dpi = 1200, height = 10, width = 10, units = "cm")
  if (!is.null(iName)) ggsave(p, filename = paste0("CO2EmiChange_SSA_10year_sensitivity_", iName, "_", format(Sys.time(), "%Y-%m-%d_%H"), ".svg"), dpi = 1200, height = 14, width = 10, units = "cm")
  
  return(list(data=iData_all, plot=p))
  
}
get_kayaDecompositionFFext2_10year  <- function(iData, MODE=NULL, CHECK_TOTALS=FALSE, RECOMPUTE_TOTALS=FALSE) {
  
  #== Initialization ===========================================================
  # Define Lapeyres function
  laspeyeres <- function(v1,v2,v3,v4,dv1,dv2,dv3,dv4) {
    
    out <- dv1*v2*v3*v4 +
      1/2*dv1*(dv2* v3* v4 +  v2*dv3* v4 + v2* v3*dv4) +
      1/3*dv1*(dv2*dv3* v4 + dv2* v3*dv4 + v2*dv3*dv4) +
      1/4*dv1*(dv2*dv3*dv4)
    
    return(out)
  }
  
  #== Recompute totals =============================================================
  if (RECOMPUTE_TOTALS) {
    iData <- iData %>% 
      dplyr::mutate(`CO2 total - coal`   = `CO2 other - coal`   + `CO2 power plant - coal (operating)`+`CO2 power plant - coal (construction)`+`CO2 power plant - coal (pre-construction)`+`CO2 power plant - coal (shelved)`) %>% 
      dplyr::mutate(`CO2 total - total`  = `CO2 total - coal`   + `CO2 total - oil` + `CO2 total - natgas` + `CO2 total - other`) %>% 
      dplyr::mutate(`PE total - coal`    = `PE other - coal`    + `PE power plant - coal (operating)`+`PE power plant - coal (construction)`+`PE power plant - coal (pre-construction)`+`PE power plant - coal (shelved)`) %>% 
      dplyr::mutate(`PE total - peat`    = `PE other - peat`    + `PE power plant - peat`) %>% 
      dplyr::mutate(`PE total - oil`     = `PE other - oil`     + `PE power plant - oil`) %>% 
      dplyr::mutate(`PE total - natgas`  = `PE other - natgas`  + `PE power plant - natgas`) %>% 
      dplyr::mutate(`PE total - nuclear` = `PE other - nuclear` + `PE power plant - nuclear`) %>% 
      dplyr::mutate(`PE total - hydro`   = `PE other - hydro`   + `PE power plant - hydro`) %>% 
      dplyr::mutate(`PE total - geo`     = `PE other - geo`     + `PE power plant - geo`) %>% 
      dplyr::mutate(`PE total - solwin`  = `PE other - solwin`  + `PE power plant - solwin`) %>% 
      dplyr::mutate(`PE total - biowas`  = `PE other - biowas`  + `PE power plant - biowas`) %>% 
      dplyr::mutate(`PE total - elec`    = `PE other - elec`    + `PE power plant - elec`) %>% 
      dplyr::mutate(`PE total - heat`    = `PE other - heat`    + `PE power plant - heat`) %>% 
      dplyr::mutate(`PE total - total`   = `PE total - coal` + `PE total - peat` + `PE total - oil` + `PE total - natgas` +
                      `PE total - nuclear` + `PE total - hydro` + `PE total - geo` + `PE total - solwin` + `PE total - biowas` + 
                      `PE total - elec` + `PE total - heat`)
  }
  
  #== Check totals =============================================================
  if (CHECK_TOTALS) {
    test <- rbind(iData %>%
                    dplyr::select(country,year,`POP`, `GDP PPP`,
                                  `CO2 total - total`, 
                                  `CO2 total - coal`,`CO2 other - coal`, `CO2 power plant - coal (operating)`,`CO2 power plant - coal (construction)`, `CO2 power plant - coal (pre-construction)`, `CO2 power plant - coal (shelved)`,
                                  `CO2 total - oil`, `CO2 total - natgas`) %>% 
                    dplyr::mutate(check_coal = `CO2 total - coal`  - (`CO2 other - coal`+`CO2 power plant - coal (operating)`+`CO2 power plant - coal (construction)`+`CO2 power plant - coal (pre-construction)`+`CO2 power plant - coal (shelved)`)) %>% 
                    dplyr::mutate(check_all  = `CO2 total - total` - (`CO2 other - coal`+`CO2 power plant - coal (operating)`+`CO2 power plant - coal (construction)`+`CO2 power plant - coal (pre-construction)`+`CO2 power plant - coal (shelved)`+`CO2 total - oil`+`CO2 total - natgas`)) %>% 
                    dplyr::mutate(type = "absolute"),
                  iData %>%
                    dplyr::select(country,year,`POP`, `GDP PPP`,
                                  `CO2 total - total`, 
                                  `CO2 total - coal`,`CO2 other - coal`, `CO2 power plant - coal (operating)`,`CO2 power plant - coal (construction)`, `CO2 power plant - coal (pre-construction)`, `CO2 power plant - coal (shelved)`,
                                  `CO2 total - oil`, `CO2 total - natgas`) %>% 
                    dplyr::mutate(check_coal = (`CO2 total - coal`  - (`CO2 other - coal`+`CO2 power plant - coal (operating)`+`CO2 power plant - coal (construction)`+`CO2 power plant - coal (pre-construction)`+`CO2 power plant - coal (shelved)`))/`CO2 total - coal`*100) %>% 
                    dplyr::mutate(check_all  = (`CO2 total - total` - (`CO2 other - coal`+`CO2 power plant - coal (operating)`+`CO2 power plant - coal (construction)`+`CO2 power plant - coal (pre-construction)`+`CO2 power plant - coal (shelved)`+`CO2 total - oil`+`CO2 total - natgas`))/`CO2 total - total`*100) %>% 
                    dplyr::mutate(type = "relative"))
    
    p <- ggplot() + 
      geom_point(aes(x=year, y=check_coal), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="black") +
      geom_point(aes(x=year, y=check_all), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="red") +
      geom_point(aes(x=year, y=check_coal), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="black") +
      geom_point(aes(x=year, y=check_all), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="red") +
      facet_wrap(~type, ncol=2) +
      theme_bw() +
      ggtitle("CO2") + ylab("Change") + xlab("")
    print(p)
    
    test <- rbind(iData %>%
                    dplyr::select(country,year,`POP`, `GDP PPP`,
                                  TPES,
                                  `PE total - total`, 
                                  `PE total - coal`,
                                  `PE other - coal`, `PE power plant - coal (operating)`,`PE power plant - coal (construction)`, `PE power plant - coal (pre-construction)`, `PE power plant - coal (shelved)`,
                                  `PE total - oil`, `PE total - natgas`, `PE total - peat`, `PE total - geo`, `PE total - biowas`, `PE total - nuclear`, `PE total - hydro`, `PE total - hydro`, `PE total - solwin`, `PE total - elec`, `PE total - heat`,
                                  `PE other - oil`, `PE other - natgas`, `PE other - peat`, `PE other - geo`, `PE other - biowas`, `PE other - nuclear`, `PE other - hydro`, `PE other - hydro`, `PE other - solwin`, `PE other - elec`, `PE other - heat`,
                                  `PE power plant - oil`, `PE power plant - natgas`, `PE power plant - peat`, `PE power plant - geo`, `PE power plant - biowas`, `PE power plant - nuclear`, `PE power plant - hydro`, `PE power plant - hydro`, `PE power plant - solwin`, `PE power plant - elec`, `PE power plant - heat`) %>% 
                    dplyr::mutate(check_coal    = `PE total - coal`    - (`PE other - coal`+`PE power plant - coal (operating)`+`PE power plant - coal (construction)`+`PE power plant - coal (pre-construction)`+`PE power plant - coal (shelved)`)) %>% 
                    dplyr::mutate(check_peat    = `PE total - peat`    - (`PE other - peat`+`PE power plant - peat`)) %>% 
                    dplyr::mutate(check_oil     = `PE total - oil`     - (`PE other - oil`+`PE power plant - oil`)) %>% 
                    dplyr::mutate(check_natgas  = `PE total - natgas`  - (`PE other - natgas`+`PE power plant - natgas`)) %>% 
                    dplyr::mutate(check_nuclear = `PE total - nuclear` - (`PE other - nuclear`+`PE power plant - nuclear`)) %>% 
                    dplyr::mutate(check_hydro   = `PE total - hydro`   - (`PE other - hydro`+`PE power plant - hydro`)) %>% 
                    dplyr::mutate(check_geo     = `PE total - geo`     - (`PE other - geo`+`PE power plant - geo`)) %>% 
                    dplyr::mutate(check_solwin  = `PE total - solwin`  - (`PE other - solwin`+`PE power plant - solwin`)) %>% 
                    dplyr::mutate(check_biowas  = `PE total - biowas`  - (`PE other - biowas`+`PE power plant - biowas`)) %>% 
                    dplyr::mutate(check_elec    = `PE total - elec`    - (`PE other - elec`+`PE power plant - elec`)) %>% 
                    dplyr::mutate(check_heat    = `PE total - heat`    - (`PE other - heat`+`PE power plant - heat`)) %>% 
                    dplyr::mutate(check_all     = `PE total - total`   - (`PE total - coal`+`PE total - peat`+`PE total - oil`+`PE total - natgas`+`PE total - nuclear`+`PE total - hydro`+`PE total - geo`+`PE total - solwin`+`PE total - biowas`+`PE total - elec`+`PE total - heat`)) %>% 
                    dplyr::mutate(type = "absolute"),
                  iData %>%
                    dplyr::select(country,year,`POP`, `GDP PPP`,
                                  TPES,
                                  `PE total - total`, 
                                  `PE total - coal`,
                                  `PE other - coal`, `PE power plant - coal (operating)`,`PE power plant - coal (construction)`, `PE power plant - coal (pre-construction)`, `PE power plant - coal (shelved)`,
                                  `PE total - oil`, `PE total - natgas`, `PE total - peat`, `PE total - geo`, `PE total - biowas`, `PE total - nuclear`, `PE total - hydro`, `PE total - hydro`, `PE total - solwin`, `PE total - elec`, `PE total - heat`,
                                  `PE other - oil`, `PE other - natgas`, `PE other - peat`, `PE other - geo`, `PE other - biowas`, `PE other - nuclear`, `PE other - hydro`, `PE other - hydro`, `PE other - solwin`, `PE other - elec`, `PE other - heat`,
                                  `PE power plant - oil`, `PE power plant - natgas`, `PE power plant - peat`, `PE power plant - geo`, `PE power plant - biowas`, `PE power plant - nuclear`, `PE power plant - hydro`, `PE power plant - hydro`, `PE power plant - solwin`, `PE power plant - elec`, `PE power plant - heat`) %>% 
                    dplyr::mutate(check_coal    = (`PE total - coal`    - (`PE other - coal`+`PE power plant - coal (operating)`+`PE power plant - coal (construction)`+`PE power plant - coal (pre-construction)`+`PE power plant - coal (shelved)`))/`PE total - coal`*100) %>% 
                    dplyr::mutate(check_peat    = (`PE total - peat`    - (`PE other - peat`+`PE power plant - peat`))/`PE total - peat`*100) %>% 
                    dplyr::mutate(check_oil     = (`PE total - oil`     - (`PE other - oil`+`PE power plant - oil`))/`PE total - oil`*100) %>% 
                    dplyr::mutate(check_natgas  = (`PE total - natgas`  - (`PE other - natgas`+`PE power plant - natgas`))/`PE total - natgas`*100) %>% 
                    dplyr::mutate(check_nuclear = (`PE total - nuclear` - (`PE other - nuclear`+`PE power plant - nuclear`))/`PE total - nuclear`*100) %>% 
                    dplyr::mutate(check_hydro   = (`PE total - hydro`   - (`PE other - hydro`+`PE power plant - hydro`))/`PE total - hydro`*100) %>% 
                    dplyr::mutate(check_geo     = (`PE total - geo`     - (`PE other - geo`+`PE power plant - geo`))/`PE total - geo`*100) %>% 
                    dplyr::mutate(check_solwin  = (`PE total - solwin`  - (`PE other - solwin`+`PE power plant - solwin`))/`PE total - solwin`*100) %>% 
                    dplyr::mutate(check_biowas  = (`PE total - biowas`  - (`PE other - biowas`+`PE power plant - biowas`))/`PE total - biowas`*100) %>% 
                    dplyr::mutate(check_elec    = (`PE total - elec`    - (`PE other - elec`+`PE power plant - elec`))/`PE total - elec`*100) %>% 
                    dplyr::mutate(check_heat    = (`PE total - heat`    - (`PE other - heat`+`PE power plant - heat`))/`PE total - heat`*100) %>% 
                    dplyr::mutate(check_all     = (`PE total - total`   - (`PE total - coal`+`PE total - peat`+`PE total - oil`+`PE total - natgas`+`PE total - nuclear`+`PE total - hydro`+`PE total - geo`+`PE total - solwin`+`PE total - biowas`+`PE total - elec`+`PE total - heat`))/`PE total - total`*100) %>%  
                    dplyr::mutate(type = "relative"))
    p <- ggplot() + 
      geom_point(aes(x=year, y=check_coal), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="darkgrey") +
      geom_point(aes(x=year, y=check_peat), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="grey") +
      geom_point(aes(x=year, y=check_oil), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="black") +
      geom_point(aes(x=year, y=check_natgas), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="lightblue") +
      geom_point(aes(x=year, y=check_nuclear), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="purple") +
      geom_point(aes(x=year, y=check_hydro), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="blue") +
      geom_point(aes(x=year, y=check_geo), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="orange") +
      geom_point(aes(x=year, y=check_solwin), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="yellow") +
      geom_point(aes(x=year, y=check_biowas), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="green") +
      geom_point(aes(x=year, y=check_elec), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="pink") +
      geom_point(aes(x=year, y=check_heat), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="red") +
      geom_point(aes(x=year, y=check_all), 
                 data=test %>% 
                   filter(type == "absolute"), 
                 color="black", bg="#ffffff00", pch=21) +
      geom_point(aes(x=year, y=check_coal), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="darkgrey") +
      geom_point(aes(x=year, y=check_peat), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="grey") +
      geom_point(aes(x=year, y=check_oil), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="black") +
      geom_point(aes(x=year, y=check_natgas), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="lightblue") +
      geom_point(aes(x=year, y=check_nuclear), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="purple") +
      geom_point(aes(x=year, y=check_hydro), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="blue") +
      geom_point(aes(x=year, y=check_geo), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="orange") +
      geom_point(aes(x=year, y=check_solwin), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="yellow") +
      geom_point(aes(x=year, y=check_biowas), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="green") +
      geom_point(aes(x=year, y=check_elec), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="pink") +
      geom_point(aes(x=year, y=check_heat), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="red") +
      geom_point(aes(x=year, y=check_all), 
                 data=test %>% 
                   filter(type == "relative"), 
                 color="black", bg="#ffffff00", pch=21) +
      facet_wrap(~type, ncol=2) +
      theme_bw() +
      ggtitle("PE") + ylab("Change") + xlab("")
    print(p)
  }
  
  #== Process data =============================================================
  #cat("Processing data...\n")
  iData <- iData %>%
    dplyr::select(country,year,`POP`, `GDP PPP`,
                  `CO2 total - total`, 
                  `CO2 total - coal`,`CO2 other - coal`, `CO2 power plant - coal (operating)`,`CO2 power plant - coal (construction)`, `CO2 power plant - coal (pre-construction)`, `CO2 power plant - coal (shelved)`,
                  `CO2 total - oil`, `CO2 total - natgas`, `CO2 total - other`,
                  `TPES`, `PE total - total`,
                  `PE total - coal`, `PE other - coal`, `PE power plant - coal (operating)`, `PE power plant - coal (construction)`, `PE power plant - coal (pre-construction)`, `PE power plant - coal (shelved)`, 
                  `PE total - oil`, `PE total - natgas`, `PE total - peat`, `PE total - nuclear`, 
                  `PE total - hydro`, `PE total - geo`, `PE total - solwin`, `PE total - heat`, `PE total - biowas`,
                  `PE total - elec`) %>% 
    dplyr::mutate(`affluence`        = `GDP PPP`           / `POP`) %>% 
    dplyr::mutate(`energy intensity` = `PE total - total`  / `GDP PPP`) %>% 
    dplyr::mutate(`carbon intensity` = `CO2 total - total` / `PE total - total`) %>% 
    dplyr::mutate(`POP`              = `POP`) %>% 
    dplyr::rename(p = `POP`) %>% 
    dplyr::rename(a = `affluence`) %>% 
    dplyr::rename(e = `energy intensity`) %>% 
    dplyr::rename(k = `carbon intensity`) %>% 
    dplyr::rename(F = `CO2 total - total`) %>% 
    dplyr::rename(emi_coal    = `CO2 total - coal`) %>% 
    dplyr::rename(emi_coal_o  = `CO2 power plant - coal (operating)`) %>%
    dplyr::rename(emi_coal_c  = `CO2 power plant - coal (construction)`) %>%
    dplyr::rename(emi_coal_pc = `CO2 power plant - coal (pre-construction)`) %>%
    dplyr::rename(emi_coal_s  = `CO2 power plant - coal (shelved)`) %>%
    dplyr::rename(emi_coal_z  = `CO2 other - coal`) %>%
    dplyr::rename(emi_oil     = `CO2 total - oil`) %>% 
    dplyr::rename(emi_gas     = `CO2 total - natgas`) %>% 
    dplyr::mutate(emi_other   = `CO2 total - other`) %>% 
    dplyr::mutate(pe_coal     = `PE total - coal`) %>% 
    dplyr::mutate(pe_coal_o   = `PE power plant - coal (operating)`) %>%
    dplyr::mutate(pe_coal_c   = `PE power plant - coal (construction)`) %>%
    dplyr::mutate(pe_coal_pc  = `PE power plant - coal (pre-construction)`) %>%
    dplyr::mutate(pe_coal_s   = `PE power plant - coal (shelved)`) %>%
    dplyr::mutate(pe_coal_z   = `PE other - coal`) %>%
    dplyr::mutate(pe_oil      = `PE total - oil`) %>% 
    dplyr::mutate(pe_gas      = `PE total - natgas`) %>%
    dplyr::mutate(pe_other    = (`PE total - peat`+`PE total - hydro` + `PE total - nuclear` + `PE total - geo` + `PE total - solwin` + `PE total - biowas` + 
                                   `PE total - heat`+`PE total - elec`)) %>% 
    dplyr::mutate(k_coal    = emi_coal     /pe_coal) %>% 
    dplyr::mutate(k_coal_o  = emi_coal_o   /pe_coal_o) %>% 
    dplyr::mutate(k_coal_c  = emi_coal_c   /pe_coal_c) %>% 
    dplyr::mutate(k_coal_pc = emi_coal_pc  /pe_coal_pc) %>% 
    dplyr::mutate(k_coal_s  = emi_coal_s   /pe_coal_s) %>% 
    dplyr::mutate(k_coal_z  = emi_coal_z   /pe_coal_z) %>% 
    dplyr::mutate(k_oil     = emi_oil      /pe_oil) %>% 
    dplyr::mutate(k_gas     = emi_gas      /pe_gas) %>% 
    dplyr::mutate(k_other   = emi_other    /pe_other) %>% 
    dplyr::mutate(pe        = `PE total - total`) %>%
    dplyr::select(country,year,p,a,e,k,F,
                  emi_coal,emi_coal_o,emi_coal_c,emi_coal_pc,emi_coal_s,emi_coal_z,emi_oil,emi_gas,emi_other,
                  pe,pe_coal,pe_coal_o,pe_coal_c,pe_coal_pc,pe_coal_s,pe_coal_z,pe_oil,pe_gas,pe_other,
                  k_coal,k_coal_o,k_coal_c,k_coal_pc,k_coal_s,k_coal_z,k_oil,k_gas,k_other) %>% 
    dplyr::filter(year %in% c(1985,1995,2005,2015,2025))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  #cat("  > Computing Laspeyres decomposition...\n")
  iData <- iData %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(year) %>% 
    dplyr::mutate(Dp         = (p-lag(p))) %>%
    dplyr::mutate(Da         = (a-lag(a))) %>%
    dplyr::mutate(De         = (e-lag(e))) %>%
    dplyr::mutate(Dk         = (k-lag(k))) %>%
    dplyr::mutate(DF         = (F-lag(F))) %>%
    dplyr::mutate(Dpecoal    = (pe_coal -lag(pe_coal))) %>%
    dplyr::mutate(Dpecoal_o  = (pe_coal_o   -0)) %>%
    dplyr::mutate(Dpecoal_c  = (pe_coal_c   -0)) %>%
    dplyr::mutate(Dpecoal_pc = (pe_coal_pc  -0)) %>%
    dplyr::mutate(Dpecoal_s  = (pe_coal_s   -0)) %>%
    dplyr::mutate(Dpecoal_z  = (pe_coal_z   -0)) %>%
    dplyr::mutate(Dpeoil     = (pe_oil  -lag(pe_oil))) %>%
    dplyr::mutate(Dpegas     = (pe_gas  -lag(pe_gas))) %>%
    dplyr::mutate(Dpeother   = (pe_other-lag(pe_other))) %>%
    dplyr::mutate(Dkc        = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    dplyr::mutate(Dkc_o      = 1/pe *(k_coal_o   *pe_coal_o   -0 - Dpecoal_o   *lag(k))) %>% 
    dplyr::mutate(Dkc_c      = 1/pe *(k_coal_c   *pe_coal_c   -0 - Dpecoal_c   *lag(k))) %>% 
    dplyr::mutate(Dkc_pc     = 1/pe *(k_coal_pc  *pe_coal_pc  -0 - Dpecoal_pc  *lag(k))) %>% 
    dplyr::mutate(Dkc_s      = 1/pe *(k_coal_s   *pe_coal_s   -0 - Dpecoal_s   *lag(k))) %>% 
    dplyr::mutate(Dkc_z      = 1/pe *(k_coal_z   *pe_coal_z   -0 - Dpecoal_z   *lag(k))) %>% 
    dplyr::mutate(Dko        = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    dplyr::mutate(Dkg        = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    dplyr::mutate(Dkr        = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    dplyr::mutate(R          = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    dplyr::mutate(ratio      = (1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De)/R) %>% 
    dplyr::mutate(Cp         = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    dplyr::mutate(Ca         = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    dplyr::mutate(Ce         = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    dplyr::mutate(Ck         = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    dplyr::mutate(Ckc        = Dkc*R) 
  if (is.null(MODE)) {
    #print("original")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Dkc_o*R)) %>%
      dplyr::mutate(Ckc_c      = Dkc_c*R) %>%
      dplyr::mutate(Ckc_pc     = Dkc_pc*R) %>%
      dplyr::mutate(Ckc_s      = Dkc_s*R) %>% 
      dplyr::mutate(Ckc_z      = Dkc_z*R)
  } else {
    #print("share")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Ckc*pe_coal_o/pe_coal)) %>% 
      dplyr::mutate(Ckc_c      = Ckc*pe_coal_c/pe_coal) %>% 
      dplyr::mutate(Ckc_pc     = Ckc*pe_coal_pc/pe_coal) %>% 
      dplyr::mutate(Ckc_s      = Ckc*pe_coal_s/pe_coal) %>% 
      dplyr::mutate(Ckc_z      = Ckc*pe_coal_z/pe_coal)
  }
  
  iData <- iData %>% 
    dplyr::mutate(Cko        = Dko*R) %>% 
    dplyr::mutate(Ckg        = Dkg*R) %>% 
    dplyr::mutate(Ckr        = Dkr*R) %>%
    dplyr::ungroup() %>% 
    tidyr::gather(variable,value,-country,-year) %>% 
    dplyr::filter(year != 1985) %>% 
    dplyr::mutate(period = factor(year, levels = c(seq(1995,2025,10)), labels=c("1985-1995",
                                                                                "1995-2005",
                                                                                "2005-2015",
                                                                                "2015-2025"), ordered=TRUE))
  
  iData_print <- iData
  #print(iData %>% dplyr::filter(variable %in% c("DF", "Cp","Ca","Ce","Ck","Ckc", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s","Cko","Ckg","Ckr","R")) %>% tidyr::spread(variable,value)) #,"Ckc_s"
  
  
  iData_all <- iData 
  
  iData <- iData %>% 
    dplyr::filter(variable %in% c("Ckc","Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s", "Cko", "Ckg", "Ckr")) %>% 
    dplyr::mutate(variable = factor(variable, c(rev(c("Ckc","Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s")), "Cko", "Ckg", "Ckr"), ordered=TRUE)) #,"Ckc_s"
  
  iDatapos <- iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg <- iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  return(list(
    "all"      = iData_all,
    "coal"     = iData,
    "coal_pos" = iDatapos,
    "coal_neg" = iDataneg,
    "print"    = iData_print %>% 
      filter(variable %in% c("DF", "Cp","Ca","Ce","Ck", "Ckc", "Ckc_z", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s","Cko","Ckg","Ckr")) %>% 
      spread(variable,value)
  ))
  
}
get_kayaDecompositionFFext2_5year   <- function(iData, MODE=NULL) {
  
  #== Initialization ===========================================================
  # Define Lapeyres function
  laspeyeres <- function(v1,v2,v3,v4,dv1,dv2,dv3,dv4) {
    
    out <- dv1*v2*v3*v4 +
      1/2*dv1*(dv2* v3* v4 +  v2*dv3* v4 + v2* v3*dv4) +
      1/3*dv1*(dv2*dv3* v4 + dv2* v3*dv4 + v2*dv3*dv4) +
      1/4*dv1*(dv2*dv3*dv4)
    
    return(out)
  }
  
  #== Process data =============================================================
  #cat("Processing data...\n")
  iData <- iData %>%
    dplyr::select(country,year,`POP`, `GDP PPP`,
                  `TPES`, `CO2 total - total`,
                  `CO2 total - coal`,`CO2 other - coal`, `CO2 power plant - coal (operating)`,`CO2 power plant - coal (construction)`, `CO2 power plant - coal (pre-construction)`, `CO2 power plant - coal (shelved)`,
                  `CO2 total - oil `, `CO2 total - natgas `,
                  `PE total - coal`, `PE other - coal`, `PE power plant - coal (operating)`, `PE Coal - Construction`, `PE Coal - Pre-Construction`, `PE Coal - Shelved`, 
                  `PE total - oil`, `PE total - natgas`, `PE total - peat`, `PE total -  nuclear`, 
                  `PE total - hydro`, `PE total - geo`, `PE total - solwin`, `PE total - heat`,
                  `PE total - elec`) %>% 
    dplyr::mutate(`affluence`             = (1e9 *`GDP PPP`)    / (1e6 *`POP`)) %>% 
    dplyr::mutate(`energy intensity`      = (1*`TPES`)          / (1e9 *`GDP PPP`)) %>% 
    dplyr::mutate(`carbon intensity`      = `CO2 total - total` / (1*`TPES`)) %>% 
    dplyr::mutate(`Population (millions)` = 1e6*`POP`) %>% 
    dplyr::rename(p = `POP`) %>% 
    dplyr::rename(a = `affluence`) %>% 
    dplyr::rename(e = `energy intensity`) %>% 
    dplyr::rename(k = `carbon intensity`) %>% 
    dplyr::rename(F = `CO2 total - total`) %>% 
    dplyr::rename(emi_coal    = `CO2 total - coal`) %>% 
    dplyr::rename(emi_coal_o  = `CO2 power plant - coal (operating)`) %>%
    dplyr::rename(emi_coal_c  = `CO2 power plant - coal (construction)`) %>%
    dplyr::rename(emi_coal_pc = `CO2 power plant - coal (pre-Construction)`) %>%
    dplyr::rename(emi_coal_s  = `CO2 power plant - coal (shelved)`) %>%
    dplyr::rename(emi_coal_z  = `CO2 other - coal`) %>%
    dplyr::rename(emi_oil     = `CO2 total - oil `) %>% 
    dplyr::rename(emi_gas     = `CO2 total - natgas `) %>% 
    dplyr::rename(emi_other   = 0.0) %>% 
    dplyr::mutate(pe_coal     = 1*(`PE total - coal`)) %>% 
    dplyr::mutate(pe_coal_o   = 1*`PE power plant - coal (operating)`) %>%
    dplyr::mutate(pe_coal_c   = 1*`PE power plant - coal (construction)`) %>%
    dplyr::mutate(pe_coal_pc  = 1*`PE power plant - coal (pre-Construction)`) %>%
    dplyr::mutate(pe_coal_s   = 1*`PE power plant - coal (shelved)`) %>%
    dplyr::mutate(pe_coal_z   = 1*`PE other - coal`) %>%
    dplyr::select(-`PE total - coal`,-`PE other - coal`,-`PE power plant - coal (operating)`,-`PE power plant - coal (construction)`,-`PE power plant - coal (pre-construction)`,-`PE power plant - coal (shelved)`) %>% 
    dplyr::mutate(pe_oil      = 1*`PE total - oil`) %>% 
    dplyr::select(-`PE total - oil`) %>% 
    dplyr::mutate(pe_gas   = 1*`PE total - natgas`) %>%
    dplyr::select(-`PE total - natgas`) %>% 
    dplyr::mutate(pe_other = 1*(`PE total - peat`+`PE total - hydro` + `PE total - nuclear` + `PE total - geo` + `PE total - solwin` + `PE total - biowas` + 
                                  `PE total - heat` + `PE total - elec`)) %>% 
    dplyr::select(-`PE total - peat`, -`PE total - hydro`, -`PE total - nuclear`, -`PE total - geo`, -`PE total - solwin`, -`PE total - biowas` + 
                    -`PE total - heat`, -`PE total - elec`) %>% 
    dplyr::mutate(k_coal    = emi_coal     /pe_coal) %>% 
    dplyr::mutate(k_coal_o  = emi_coal_o   /pe_coal_o) %>% 
    dplyr::mutate(k_coal_c  = emi_coal_c   /pe_coal_c) %>% 
    dplyr::mutate(k_coal_pc = emi_coal_pc  /pe_coal_pc) %>% 
    dplyr::mutate(k_coal_s  = emi_coal_s   /pe_coal_s) %>% 
    dplyr::mutate(k_oil     = emi_oil      /pe_oil) %>% 
    dplyr::mutate(k_gas     = emi_gas      /pe_gas) %>% 
    dplyr::mutate(k_other   = emi_other    /pe_other) %>% 
    dplyr::mutate(pe        = 1*`TPES`) %>%
    dplyr::select(country,year,p,a,e,k,F,
                  emi_coal,emi_coal_o,emi_coal_c,emi_coal_pc,emi_coal_s,emi_coal_z,emi_oil,emi_gas,emi_other,
                  pe,pe_coal,pe_coal_o,pe_coal_c,pe_coal_pc,pe_coal_s,pe_coal_z,pe_oil,pe_gas,pe_other,
                  k_coal,k_coal_o,k_coal_c,k_coal_pc,k_coal_s,k_oil,k_gas,k_other) %>% 
    dplyr::filter(year %in% seq(1985,2020,5))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  #cat("  > Computing Laspeyres decomposition...\n")
  iData <- iData %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(year) %>% 
    dplyr::mutate(Dp         = (p-lag(p))) %>%
    dplyr::mutate(Da         = (a-lag(a))) %>%
    dplyr::mutate(De         = (e-lag(e))) %>%
    dplyr::mutate(Dk         = (k-lag(k))) %>%
    dplyr::mutate(DF         = (F-lag(F))) %>%
    dplyr::mutate(Dpecoal    = (pe_coal    -lag(pe_coal))) %>%
    dplyr::mutate(Dpecoal_o  = (pe_coal_o  -0)) %>%
    dplyr::mutate(Dpecoal_c  = (pe_coal_c  -0)) %>%
    dplyr::mutate(Dpecoal_pc = (pe_coal_pc -0)) %>%
    dplyr::mutate(Dpecoal_s  = (pe_coal_s  -0)) %>%
    dplyr::mutate(Dpecoal_z  = (pe_coal_z  -0)) %>%
    dplyr::mutate(Dpeoil     = (pe_oil     -lag(pe_oil))) %>%
    dplyr::mutate(Dpegas     = (pe_gas     -lag(pe_gas))) %>%
    dplyr::mutate(Dpeother   = (pe_other   -lag(pe_other))) %>%
    dplyr::mutate(Dkc        = 1/pe *(k_coal     *pe_coal    -lag(k_coal) *lag(pe_coal)  - Dpecoal    *lag(k))) %>% 
    dplyr::mutate(Dkc_o      = 1/pe *(k_coal_o   *pe_coal_o  -0                          - Dpecoal_o  *lag(k))) %>% 
    dplyr::mutate(Dkc_c      = 1/pe *(k_coal_c   *pe_coal_c  -0                          - Dpecoal_c  *lag(k))) %>% 
    dplyr::mutate(Dkc_pc     = 1/pe *(k_coal_pc  *pe_coal_pc -0                          - Dpecoal_pc *lag(k))) %>% 
    dplyr::mutate(Dkc_s      = 1/pe *(k_coal_s   *pe_coal_s  -0                          - Dpecoal_s  *lag(k))) %>% 
    dplyr::mutate(Dkc_z      = 1/pe *(k_coal_z   *pe_coal_z  -0                          - Dpecoal_z  *lag(k))) %>% 
    dplyr::mutate(Dko        = 1/pe *(k_oil      *pe_oil     -lag(k_oil)  *lag(pe_oil)   - Dpeoil     *lag(k))) %>% 
    dplyr::mutate(Dkg        = 1/pe *(k_gas      *pe_gas     -lag(k_gas)  *lag(pe_gas)   - Dpegas     *lag(k))) %>% 
    dplyr::mutate(Dkr        = 1/pe *(k_other    *pe_other   -lag(k_other)*lag(pe_other) - Dpeother   *lag(k))) %>%
    dplyr::mutate(R          = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    dplyr::mutate(Cp         = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    dplyr::mutate(Ca         = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    dplyr::mutate(Ce         = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    dplyr::mutate(Ck         = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    dplyr::mutate(Ckc        = Dkc   *R) 
  
  if (is.null(MODE)) {
    #print("original")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Dkc_o*R)) %>%
      dplyr::mutate(Ckc_c      = Dkc_c*R) %>%
      dplyr::mutate(Ckc_pc     = Dkc_pc*R) %>%
      dplyr::mutate(Ckc_s      = Dkc_s*R) %>% 
      dplyr::mutate(Ckc_z      = Dkc_z*R)
  } else {
    #print("share")
    iData <- iData %>% 
      dplyr::mutate(Ckc_o      = ifelse(year != 2025, Ckc, Ckc*pe_coal_o/pe_coal)) %>% 
      dplyr::mutate(Ckc_c      = Ckc*pe_coal_c/pe_coal) %>% 
      dplyr::mutate(Ckc_pc     = Ckc*pe_coal_pc/pe_coal) %>% 
      dplyr::mutate(Ckc_s      = Ckc*pe_coal_s/pe_coal) %>% 
      dplyr::mutate(Ckc_z      = Ckc*pe_coal_z/pe_coal)
  }
  iData <- iData %>% 
    dplyr::ungroup() %>% 
    tidyr::gather(variable,value,-country,-year) %>% 
    dplyr::filter(year != 1985) %>% 
    dplyr::mutate(period = factor(year, levels = c(seq(1990,2020,5)), labels=c("1985-1990",
                                                                               "1990-1995",
                                                                               "1995-2000",
                                                                               "2000-2005",
                                                                               "2005-2010",
                                                                               "2010-2015",
                                                                               "2015-2020"), ordered=TRUE))
  
  iData_print = iData
  print(iData %>% dplyr::filter(variable %in% c("DF", "Cp","Ca","Ce","Ck","Ckc", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s","Ckc_z","Cko","Ckg","Ckr","R")) %>% tidyr::spread(variable,value)) #,"Ckc_s"
  
  iData_all = iData
  
  iData = iData %>% dplyr::mutate(variable = factor(variable, c(rev(c("Ckc","Ckc_z","Ckc_o","Ckc_c","Ckc_pc","Ckc_s")), "Cko", "Ckg", "Ckr"), ordered=TRUE)) #,"Ckc_s"
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  return(list(
    "all"      = iData_all,
    "coal"     = iData,
    "coal_pos" = iDatapos,
    "coal_neg" = iDataneg,
    "print"    = iData_print %>% filter(variable %in% c("DF", "Cp","Ca","Ce","Ck", "Ckc","Ckc_z", "Ckc_o","Ckc_c","Ckc_pc","Ckc_s","Cko","Ckg","Ckr")) %>% spread(variable,value)
  ))
  
}
process_data <- function(iDataCoal, iDataOther, iCountry, iDataLF, iDataEF, iDataEmi, iLF, iRpc, iRs) {
  #-- Initialise ------
  tmp_PE_Emi <- list()
  
  #-- Get efficiency, load factors and emission factors from distributions -----
  tmp_efficiency <- iDataEF
  tmp_loadfactor <- iDataLF
  tmp_emifactor  <- iDataEmi
  
  #-- Update coal load factor ------
  tmp_loadfactor[["coal"]] <- iLF
  
  #-- Process power plant capacity data ------
  # Coal capacities (data at plant level + status) (uncertainty mostly in load factor and status, a bit on efficiency and emission factor)
  tmp_PE_Emi$coal <- compute_coaldata(iDataCoal, tmp_loadfactor$coal, tmp_efficiency$coal, tmp_emifactor$coal)
  
  #-- Process other power plant capacity data ------
  for (k_fuel in c("oil", "gas", "bio", "ren", "hydro", "geo", "nuc")) {
    tmp_PE_Emi[[k_fuel]] <- compute_otherdata(iDataOther[[k_fuel]], k_fuel, tmp_loadfactor[[k_fuel]], tmp_efficiency[[k_fuel]], tmp_emifactor[[k_fuel]], iCountry)
  }
  
  # Merge coal, oil, gas, hydro , ren and nuc data
  pp_cap_mc <- merge_allData(iCountry, tmp_PE_Emi)
  
  # Sum up data (over African countries)
  out_data_mc <- pp_cap_mc %>% 
    select(-country, -iso, -COUNTRY_PLATTS, -COUNTRY_COALSWARM) %>% 
    colSums() %>% 
    data.frame(value=.) %>% 
    tibble::rownames_to_column("variable") %>% 
    select(variable, value) %>% 
    spread(variable, value)
  
  #-- Post-Process data ------------
  out_data <- postprocess_data(out_data_mc)
  
  #-- Format data ------------------
  out_data_fmt <- out_data[,which(!grepl("retired", names(out_data)))]
  for (k_fuel in c("oil", "natgas", "biowas", "solwin", "hydro", "geo", "nuclear")) {
    cur_pe_names <- grep(paste0("PE power plant - ", k_fuel), names(out_data_fmt), value=TRUE)
    cur_co2_names <- grep(paste0("CO2 power plant - ", k_fuel), names(out_data_fmt), value=TRUE)
    out_data_fmt[[paste0("PE power plant - ", k_fuel)]] <- sum(out_data_fmt[, cur_pe_names])
    out_data_fmt[[paste0("CO2 power plant - ", k_fuel)]] <- sum(out_data_fmt[, cur_co2_names])
    out_data_fmt <- out_data_fmt[, -which(names(out_data_fmt) %in% c(cur_pe_names, cur_co2_names))]  
  }
  for (k_miss in names(data_all_coalcats_ssa)[which(!names(data_all_coalcats_ssa) %in% names(out_data_fmt))]) {
    out_data_fmt[[k_miss]] <- NA
  }
  
  out_data_fmt <- out_data_fmt[, names(data_all_coalcats_ssa)]
  
  #format_data(out_data)
  #print(out_data_fmt$`PE Natural Gas`[nrow(out_data_fmt)])
  
  
  #-- Bind IEA2017 and PLATTS/BB2018 data ------
  p_data <- rbind(data_all_coalcats_ssa %>%
                    filter(year >= 1988, year <= 2015),
                  out_data_fmt)  %>% 
    mutate(country = "Sub-Saharan Africa") 
  
  #-- Adjust data -----------------
  p_data_final <- adjust_data(p_data, tmp_emifactor, iRpc, iRs) 
  #print(p_data$`PE Natural Gas`[nrow(p_data)])
  
  return(p_data_final)
  
}