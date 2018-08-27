#==============================
# USER SECTION
#==============================
# Notes:
#  - Only considering coal and gas power plants under construction at the moment
#  - Hydro capacities are aggregated into planned and under construction at the moment

# Time
u_year                <- 2025
u_coal_loadFactor     <- seq(0.2,0.7,0.05)
u_ratio_coal_preccons <- seq(0.0,1.0,0.5)
u_ratio_coal_shelved  <- seq(0.0,1.0,0.5)
u_ratios_coal_pc_s    <- seq(0.0, 2.0, 0.05)
u_getandpprocdata     <- TRUE
u_countries           <- c('Angola', 'Benin', 'Botswana', 'Cameroon', 'Congo', "Côte d'Ivoire", 'Democratic Republic of the Congo', 
                           'Eritrea', 'Ethiopia', 'Gabon', 'Ghana', 'Kenya', 'Mauritius', 'Mozambique', 'Namibia', 'Niger', 
                           'Nigeria', 'Senegal', 'South Sudan', 'Sudan', 'Tanzania', 'Togo', 'Zambia', 'Zimbabwe', 'Other Africa')

#-- Unit conversion factor -------------------
EJ_2_ktoe  <- 23884.5897    # IEA unit converter website (https://www.iea.org/statistics/resources/unitconverter/; last accessed 20180604)
ktoe_2_kWh <- 11630000.0

#-- Conversion efficiency factors -------
# (p.46 https://www.ise.fraunhofer.de/content/dam/ise/en/documents/publications/studies/recent-facts-about-photovoltaics-in-germany.pdf)
data_ef <- list()
data_ef$coal   <- 0.38 # IEA (2010) Worldwide coal-fired power plant efficiency averaged 35.1% in 2007, compared with 33.5% in 1971. (https://www.iea.org/ciab/papers/power_generation_from_coal.pdf)
data_ef$oil    <- 0.40 # 
data_ef$gas    <- 0.49 # 
data_ef$bio    <- 1.00 # 
data_ef$ren    <- 1.00 # 
data_ef$hydro  <- 1.00 # 
data_ef$nuc    <- 0.33 # 
data_ef$geo    <- 1.00 # 

#-- Load factors / Capacity factors ---------
# !! VERY IMPORTANT/SENSITIVE PARAMETER !!
# Average load factor computed over 2010-2015 (combining PLATTS and IEA data)
data_lf <- list()
data_lf$coal   <- 0.48 # OK
data_lf$oil    <- 0.07 # 
data_lf$gas    <- 0.52 # OK
data_lf$bio    <- 0.03 # 
data_lf$ren    <- 0.57 # Pretty high compared to Germany (p.42 https://www.ise.fraunhofer.de/content/dam/ise/en/documents/publications/studies/recent-facts-about-photovoltaics-in-germany.pdf)
data_lf$hydro  <- 0.71 # OK
data_lf$nuc    <- 0.75 # Using world average
data_lf$geo    <- 0.88 # OK

#-- Emission factors [MtCO2/ktoe] ----------
# Estimates from IEA 2017 WEB and CO2 data (mean in SSA over 2000-2015)
data_emi <- list()
data_emi$coal   <- ef_all_mean2000_2015$value[ef_all_mean2000_2015$product == "coal"   & ef_all_mean2000_2015$category == "ef_pp"]
data_emi$oil    <- ef_all_mean2000_2015$value[ef_all_mean2000_2015$product == "oil"    & ef_all_mean2000_2015$category == "ef_pp"]
data_emi$gas    <- ef_all_mean2000_2015$value[ef_all_mean2000_2015$product == "natgas" & ef_all_mean2000_2015$category == "ef_pp"]
data_emi$bio    <- 0.0
data_emi$ren    <- 0.0
data_emi$hydro  <- 0.0
data_emi$nuc    <- 0.0
data_emi$geo    <- 0.0

# Good agreement with IPCC Guidelines 2006
# https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/2_Volume2/V2_2_Ch2_Stationary_Combustion.pdf
cat("Emission factors [gCO2/kWh]:\n")
cat(paste0(" - Coal: ", round(data_emi$coal/(data_ef$coal*ktoe_2_kWh)*1e12), "\n"))
cat(paste0(" - Oil: ", round(data_emi$oil/(data_ef$oil*ktoe_2_kWh)*1e12), "\n"))
cat(paste0(" - Natural gas: ", round(data_emi$gas/(data_ef$gas*ktoe_2_kWh)*1e12), "\n"))