plot_kayaDecomposition <- function(iData, i_ylim=c(-140,300)) {
  
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
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (PJ)`, `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e15*`Total Primary Energy Supply (PJ)`)  / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e15*`Total Primary Energy Supply (PJ)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    select(country,year,p,a,e,k,F) %>% 
    filter(year %in% seq(1990,2020,5))
  
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
    mutate(Cp = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) %>% 
    filter(year != 1990) %>% 
    mutate(period = factor(year, levels = c(seq(1995,2020,5)), labels=c("1990-1995",
                                                                        "1995-2000",
                                                                        "2000-2005",
                                                                        "2005-2010",
                                                                        "2010-2015",
                                                                        "2015-2020"), ordered=TRUE))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(4, "Set1")
  names(cols)  = c("Cp", "Ca", "Ce", "Ck")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ck"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity"))
  
  #-- Plot ---------------------------------------------------------------------
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period != "1990-1995", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period != "1990-1995", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-1995", variable == "DF"), color="white", fill="white", size=3) +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-1995", variable == "DF"))
  
  # Formatting
  p = p +
    colour_scale +
    ylim(i_ylim) +
    ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
}

plot_kayaDecomposition_19702015 <- function(iData, i_ylim=c(-140,300)) {
  
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
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (PJ)`, `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e15*`Total Primary Energy Supply (PJ)`)  / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e15*`Total Primary Energy Supply (PJ)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    select(country,year,p,a,e,k,F) %>% 
    filter(year %in% seq(1970,2015,1))
  
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
    mutate(Cp = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) %>% 
    filter(year != 1970) %>% 
    mutate(period = factor(year, levels = c(seq(1971,2015,1)), ordered=TRUE))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(4, "Set1")
  names(cols)  = c("Cp", "Ca", "Ce", "Ck")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ck"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity"))
  
  #-- Plot ---------------------------------------------------------------------
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period != "1990-1995", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period != "1990-1995", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-1995", variable == "DF"), color="white", fill="white", size=3) +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-1995", variable == "DF"))
  
  # Formatting
  p = p +
    colour_scale +
    ylim(i_ylim) +
    ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
}

plot_kayaDecomposition_19702015_rel <- function(iData, i_ylim=c(-140,300), title="", LEGEND=FALSE) {
  
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
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (PJ)`, `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e15*`Total Primary Energy Supply (PJ)`)  / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e15*`Total Primary Energy Supply (PJ)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    select(country,year,p,a,e,k,F) %>% 
    filter(year %in% seq(1970,2015,1))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(country) %>%
    arrange(year) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))/lag(F)*100) %>%
    mutate(Cp = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/lag(F)*100) %>%
    mutate(Ca = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/lag(F)*100) %>%
    mutate(Ce = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/lag(F)*100) %>%
    mutate(Ck = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/lag(F)*100) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) #%>% 
  #filter(year != 1970) #%>% 
  #mutate(period = factor(year, levels = c(seq(1971,2015,1)), ordered=TRUE))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(4, "Set1")
  cols = c("#ffa07aff", "#ffec78ff", "#b0e2ffff", "#b3ee3aff")
  names(cols)  = c("Cp", "Ca", "Ce", "Ck")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ck"),
    labels = c("Population", "GDP per capita", "Energy intensity", "Carbon intensity"))
  
  #-- Plot ---------------------------------------------------------------------
  p = ggplot()
  p = p +
    geom_bar(aes(x=year, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) %>% mutate(variable=factor(variable, levels=rev(c("Cp","Ca","Ce","Ck")), ordered=TRUE)) , stat="identity", position="stack") +
    geom_bar(aes(x=year, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) %>% mutate(variable=factor(variable, levels=rev(c("Cp","Ca","Ce","Ck")), ordered=TRUE)), stat="identity", position="stack") +
    geom_point(aes(x=year, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
    geom_point(aes(x=year, y=value), data=iData %>% filter(variable == "DF"))
  
  # Formatting
  p = p +
    colour_scale +
    ggtitle(title) +
    scale_x_continuous(name = "", breaks = seq(1970, 2015, 5)) +
    ylim(i_ylim) +
    ylab(expression(paste(Delta, " CO2 per year [%]"))) + xlab("") +
    theme_bw() +
    theme(text=element_text(size=18)) #, axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  
  
  if (LEGEND==TRUE) {
    p = p + theme(legend.position = c(0.7, 0.1)) + 
      guides(fill=guide_legend(nrow=2,byrow=TRUE))
  } else {
    p = p + theme(legend.position = "none")
  }
  
  print(p)
  
}

get_kayaDecompositionFF_20012015_rel <- function(iData, startyear=2001, endyear=2015) {
  
  #== Initialization ===========================================================
  # Define Lapeyres function
  laspeyeres <- function(v1,v2,v3,v4,dv1,dv2,dv3,dv4) {
    
    out <- dv1*v2*v3*v4 +
      1/2*dv1*(dv2* v3* v4 +  v2*dv3* v4 + v2* v3*dv4) +
      1/3*dv1*(dv2*dv3* v4 + dv2* v3*dv4 + v2*dv3*dv4) +
      1/4*dv1*(dv2*dv3*dv4)
    
    return(out)
  }
  
  dt=endyear-startyear
  
  #== Process data =============================================================
  cat("Processing data...\n")
  iData2 = iData %>%
    filter(year %in% c(startyear,endyear)) %>% 
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (ktoe)`, `CO2 Sectoral Approach (Mt of CO2)`,
           `CO2 Coal and Coal Products`, `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `,
           `PE Coal and Coal Products`, `PE Crude, NGL and Feedstocks`, `PE Petroleum Products`, `PE Natural Gas`, `PE Peat`, `PE Nuclear`, 
           `PE Hydro`, `PE Geothermal`, `PE Solar/Wind/Other`, `PE Combustible Renewables and Waste`, `PE Heat Production from non-specified comb.fuels`,
           `PE Electricity`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e3*`Total Primary Energy Supply (ktoe)`) / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e3*`Total Primary Energy Supply (ktoe)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(emi_coal  = `CO2 Coal and Coal Products`) %>% 
    rename(emi_oil   = `CO2 Oil `) %>% 
    rename(emi_gas   = `CO2 Natural Gas `) %>% 
    mutate(emi_nuclear   = 0.0) %>%
    rename(emi_biowaste   = `CO2 Other `) %>%
    mutate(emi_other = 0.0) %>% 
    mutate(pe_coal  = 1e3*(`PE Coal and Coal Products` + `PE Peat`)) %>% 
    select(-`PE Coal and Coal Products`, -`PE Peat`) %>% 
    mutate(pe_oil   = 1e3*(`PE Crude, NGL and Feedstocks` + `PE Petroleum Products`)) %>% 
    select(-`PE Crude, NGL and Feedstocks`, -`PE Petroleum Products`) %>% 
    mutate(pe_gas   = 1e3*`PE Natural Gas`) %>%
    select(-`PE Natural Gas`) %>% 
    mutate(pe_nuclear   = 1e3*`PE Nuclear`) %>%
    select(-`PE Nuclear`) %>% 
    mutate(pe_biowaste   = 1e3*`PE Combustible Renewables and Waste`) %>%
    select(-`PE Combustible Renewables and Waste`) %>% 
    mutate(pe_other = 1e3*(`PE Hydro` + `PE Geothermal` + `PE Solar/Wind/Other`)) %>%  #`PE Heat Production from non-specified comb.fuels` + `PE Electricity`
    select(-`PE Hydro`,-`PE Geothermal`,-`PE Solar/Wind/Other`,
           -`PE Heat Production from non-specified comb.fuels`,-`PE Electricity`) %>% 
    mutate(k_coal  = ifelse(pe_coal==0, 0, emi_coal /pe_coal)) %>%
    mutate(k_oil   = ifelse(pe_oil==0, 0, emi_oil  /pe_oil)) %>%
    mutate(k_gas   = ifelse(pe_gas==0, 0, emi_gas  /pe_gas)) %>%
    mutate(k_nuclear  = ifelse(pe_nuclear==0, 0, emi_nuclear  /pe_nuclear)) %>%
    mutate(k_biowaste = ifelse(pe_biowaste==0, 0, emi_biowaste  /pe_biowaste)) %>%
    mutate(k_other = ifelse(pe_other==0, 0, emi_other/pe_other)) %>%
    # mutate(k_coal  = emi_coal /pe_coal) %>% 
    # mutate(k_oil   = emi_oil  /pe_oil) %>% 
    # mutate(k_gas   = emi_gas  /pe_gas) %>% 
    # mutate(k_nuclear  = emi_nuclear  /pe_nuclear) %>%
    # mutate(k_biowaste = emi_biowaste  /pe_biowaste) %>%
    mutate(k_other = emi_other/pe_other) %>% 
    mutate(pe      = 1e3*`Total Primary Energy Supply (ktoe)`) %>%
    select(country,year,p,a,e,k,F,
           emi_coal,emi_oil,emi_gas,emi_nuclear,emi_biowaste,emi_other,
           pe,
           pe_coal,pe_oil,pe_gas,pe_nuclear,pe_biowaste,pe_other,
           k_coal,k_oil,k_gas,k_nuclear,k_biowaste,k_other)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData3 = iData2 %>%
    group_by(country) %>%
    arrange(year) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF  = (F-lag(F))/dt) %>%
    mutate(coeff_rel=1/lag(F)*100) %>% 
    mutate(DFr = (F-lag(F))/lag(F)*100/dt) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpenuclear  = (pe_nuclear  -lag(pe_nuclear))) %>%
    mutate(Dpebiowaste = (pe_biowaste  -lag(pe_biowaste))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkn = 1/pe *(k_nuclear  *pe_nuclear   - lag(k_nuclear)  *lag(pe_nuclear)   - Dpenuclear  *lag(k))) %>% 
    mutate(Dkb = 1/pe *(k_biowaste  *pe_biowaste   - lag(k_biowaste)  *lag(pe_biowaste)   - Dpebiowaste  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/dt) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/dt) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/dt) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/dt) %>%
    mutate(Cp_r  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/lag(F)*100/dt) %>%
    mutate(Ca_r  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/lag(F)*100/dt) %>%
    mutate(Ce_r  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/lag(F)*100/dt) %>%
    mutate(Ck_r  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/lag(F)*100/dt) %>%
    mutate(Ckc = Dkc*R/dt) %>% 
    mutate(Cko = Dko*R/dt) %>% 
    mutate(Ckg = Dkg*R/dt) %>% 
    mutate(Ckn = Dkn*R/dt) %>% 
    mutate(Ckb = Dkb*R/dt) %>% 
    mutate(Ckr = Dkr*R/dt) %>%
    mutate(Ckcr = Dkc*R/lag(F)*100/dt) %>% 
    mutate(Ckor = Dko*R/lag(F)*100/dt) %>% 
    mutate(Ckgr = Dkg*R/lag(F)*100/dt) %>% 
    mutate(Cknr = Dkn*R/lag(F)*100/dt) %>% 
    mutate(Ckbr = Dkb*R/lag(F)*100/dt) %>% 
    mutate(Ckrr = Dkr*R/lag(F)*100/dt) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) %>% 
    filter(variable %in% c("DF", "DFr", "Cp_r", "Ca_r", "Ce_r", "Ck_r", "Ckcr", "Ckor", "Ckgr", "Cknr", "Ckbr", "Ckrr", "Cp", "Ca", "Ce", "Ck")) %>% 
    mutate(value=round(value, digits = 1)) %>% 
    spread(variable, value) %>% 
    filter(year==endyear) %>% 
    rename(period=year) %>% 
    mutate(period=paste0(startyear,"-", endyear)) %>% 
    rename(dCO2=DFr) %>% 
    rename(p=Cp_r, a=Ca_r, e=Ce_r, k=Ck_r) %>% 
    rename(Coal=Ckcr, Oil=Ckor, NaG=Ckgr, Nuc=Cknr, RE=Ckrr, Bio=Ckbr) %>% 
    group_by(period) %>% 
    arrange(desc(DF)) %>% 
    mutate(rank_DF=row_number()) %>%
    arrange(desc(dCO2)) %>% 
    mutate(rank_dCO2=row_number()) %>% 
    arrange(desc(k)) %>% 
    mutate(rank_k=row_number()) %>% 
    ungroup() %>% 
    mutate(rank = paste0(rank_k, " (", rank_dCO2, ")")) %>% 
    select(rank, country, dCO2, p, a, e, k, Coal, Oil, NaG, Nuc, RE, Bio) #, rank_DF, DF, period
  
  return(iData3)
  
}

plot_kayaDecompositionFF_19702015_rel <- function(iData, i_ylim=c(-140,300), title="", LEGEND=FALSE) {
  
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
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (ktoe)`, `CO2 Sectoral Approach (Mt of CO2)`,
           `CO2 Coal and Coal Products`, `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `,
           `PE Coal and Coal Products`, `PE Crude, NGL and Feedstocks`, `PE Petroleum Products`, `PE Natural Gas`, `PE Peat`, `PE Nuclear`, 
           `PE Hydro`, `PE Geothermal`, `PE Solar/Wind/Other`, `PE Combustible Renewables and Waste`, `PE Heat Production from non-specified comb.fuels`,
           `PE Electricity`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e3*`Total Primary Energy Supply (ktoe)`) / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e3*`Total Primary Energy Supply (ktoe)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(emi_coal  = `CO2 Coal and Coal Products`) %>% 
    rename(emi_oil   = `CO2 Oil `) %>% 
    rename(emi_gas   = `CO2 Natural Gas `) %>% 
    mutate(emi_nuclear   = 0.0) %>%
    rename(emi_biowaste   = `CO2 Other `) %>%
    mutate(emi_other = 0.0) %>% 
    mutate(pe_coal  = 1e3*(`PE Coal and Coal Products` + `PE Peat`)) %>% 
    select(-`PE Coal and Coal Products`, -`PE Peat`) %>% 
    mutate(pe_oil   = 1e3*(`PE Crude, NGL and Feedstocks` + `PE Petroleum Products`)) %>% 
    select(-`PE Crude, NGL and Feedstocks`, -`PE Petroleum Products`) %>% 
    mutate(pe_gas   = 1e3*`PE Natural Gas`) %>%
    select(-`PE Natural Gas`) %>% 
    mutate(pe_nuclear   = 1e3*`PE Nuclear`) %>%
    select(-`PE Nuclear`) %>% 
    mutate(pe_biowaste   = 1e3*`PE Combustible Renewables and Waste`) %>%
    select(-`PE Combustible Renewables and Waste`) %>% 
    mutate(pe_other = 1e3*(`PE Hydro` + `PE Geothermal` + `PE Solar/Wind/Other`)) %>%  #`PE Heat Production from non-specified comb.fuels` + `PE Electricity`
    select(-`PE Hydro`,-`PE Geothermal`,-`PE Solar/Wind/Other`,
           -`PE Heat Production from non-specified comb.fuels`,-`PE Electricity`) %>% 
    mutate(k_coal  = emi_coal /pe_coal) %>% 
    mutate(k_oil   = emi_oil  /pe_oil) %>% 
    mutate(k_gas   = emi_gas  /pe_gas) %>% 
    mutate(k_nuclear  = emi_nuclear  /pe_nuclear) %>%
    mutate(k_biowaste = emi_biowaste  /pe_biowaste) %>%
    mutate(k_other = emi_other/pe_other) %>% 
    mutate(pe      = 1e3*`Total Primary Energy Supply (ktoe)`) %>%
    select(country,year,p,a,e,k,F,
           emi_coal,emi_oil,emi_gas,emi_nuclear,emi_biowaste,emi_other,
           pe,
           pe_coal,pe_oil,pe_gas,pe_nuclear,pe_biowaste,pe_other,
           k_coal,k_oil,k_gas,k_nuclear,k_biowaste,k_other)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(country) %>%
    arrange(year) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))/lag(F)*100) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpenuclear  = (pe_nuclear  -lag(pe_nuclear))) %>%
    mutate(Dpebiowaste = (pe_biowaste  -lag(pe_biowaste))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkn = 1/pe *(k_nuclear  *pe_nuclear   - lag(k_nuclear)  *lag(pe_nuclear)   - Dpenuclear  *lag(k))) %>% 
    mutate(Dkb = 1/pe *(k_biowaste  *pe_biowaste   - lag(k_biowaste)  *lag(pe_biowaste)   - Dpebiowaste  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/lag(F)*100) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/lag(F)*100) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/lag(F)*100) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/lag(F)*100) %>%
    mutate(Ckc = Dkc*R/lag(F)*100) %>% 
    mutate(Cko = Dko*R/lag(F)*100) %>% 
    mutate(Ckg = Dkg*R/lag(F)*100) %>% 
    mutate(Ckn = Dkn*R/lag(F)*100) %>% 
    mutate(Ckb = Dkb*R/lag(F)*100) %>% 
    mutate(Ckr = Dkr*R/lag(F)*100) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) #%>% 
  #filter(year != 1970) #%>% 
  #mutate(period = factor(year, levels = c(seq(1971,2015,1)), ordered=TRUE))
  
  print(iData %>% filter(variable %in% c("Cp","Ca","Ce","Ckc","Cko","Ckg","Ckn","Ckb","Ckr")) %>% spread(variable,value))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(7, "Set1")
  cols = c("#8b5a00ff", "#000000ff", "#ff7155ff", "#ffd600ff", "#65cd00ff", "#0000ffff")
  names(cols)  = c("Ckc", "Cko", "Ckg", "Ckn", "Ckb", "Ckr")
  colour_scale = scale_fill_manual(
    name   = "",
    values = cols,
    breaks = c("Ckc", "Cko", "Ckg", "Ckn", "Ckb", "Ckr"),
    labels = c("Coal", "Crude Oil", "Natural Gas", "Nuclear", "Biomass and Waste", "Renewables (incl. hydro)"))
  
  #-- Plot ---------------------------------------------------------------------
  p = ggplot()
  p = p +
    geom_bar(aes(x=year, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckn", "Ckb", "Ckr")) %>% mutate(variable=factor(variable, levels=rev(c("Ckc", "Cko", "Ckg", "Ckn", "Ckb", "Ckr")), ordered=TRUE)) , stat="identity", position="stack") +
    geom_bar(aes(x=year, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckn", "Ckb", "Ckr")) %>% mutate(variable=factor(variable, levels=rev(c("Ckc", "Cko", "Ckg", "Ckn", "Ckb", "Ckr")), ordered=TRUE)), stat="identity", position="stack") +
    geom_point(aes(x=year, y=value), data=iData %>% filter(variable == "Ck"), color="white", fill="white", size=3) +
    geom_point(aes(x=year, y=value), data=iData %>% filter(variable == "Ck"))
  
  # Formatting
  p = p +
    colour_scale +
    ggtitle(title) +
    scale_x_continuous(name = "", breaks = seq(1970, 2015, 5)) +
    ylim(i_ylim) +
    ylab(expression(paste(Delta, " CO2 per year [%]"))) + xlab("") +
    theme_bw() +
    theme(text=element_text(size=18)) #, axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  
  
  if (LEGEND==TRUE) {
    p = p + theme(legend.position = c(0.5, 0.1)) + 
      guides(fill=guide_legend(nrow=2,byrow=TRUE))
  } else {
    p = p + theme(legend.position = "none")
  }
  
  print(p)
  
}

plot_kayaDecomposition_rel <- function(iData, i_ylim=c(-140,300)) {
  
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
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (PJ)`, `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e15*`Total Primary Energy Supply (PJ)`)  / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e15*`Total Primary Energy Supply (PJ)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    select(country,year,p,a,e,k,F) %>% 
    filter(year %in% seq(1990,2020,5))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(country) %>%
    arrange(year) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))/lag(F)*100/5) %>%
    mutate(Cp = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/lag(F)*100/5) %>%
    mutate(Ca = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/lag(F)*100/5) %>%
    mutate(Ce = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/lag(F)*100/5) %>%
    mutate(Ck = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/lag(F)*100/5) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) %>% 
    filter(year != 1990) %>% 
    mutate(period = factor(year, levels = c(seq(1995,2020,5)), labels=c("1990-1995",
                                                                        "1995-2000",
                                                                        "2000-2005",
                                                                        "2005-2010",
                                                                        "2010-2015",
                                                                        "2015-2020"), ordered=TRUE))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(4, "Set1")
  names(cols)  = c("Cp", "Ca", "Ce", "Ck")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ck"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity"))
  
  #-- Plot ---------------------------------------------------------------------
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period != "1990-1995", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period != "1990-1995", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-1995", variable == "DF"), color="white", fill="white", size=3) +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-1995", variable == "DF"))
  
  # Formatting
  p = p +
    colour_scale +
    #ylim(i_ylim) +
    ylab("Change in CO2 emissions [%]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
}

plot_kayaDecomposition_rel_10year <- function(iData, i_ylim=c(-140,300)) {
  
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
    select(country,year,`Population (millions)`, `GDP (billion 2000 US$ using PPPs)`,
           `Total Primary Energy Supply (PJ)`, `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    mutate(`affluence`             = (1e9 *`GDP (billion 2000 US$ using PPPs)`) / (1e6 *`Population (millions)`)) %>% 
    mutate(`energy intensity`      = (1e15*`Total Primary Energy Supply (PJ)`)  / (1e9 *`GDP (billion 2000 US$ using PPPs)`)) %>% 
    mutate(`carbon intensity`      = `CO2 Sectoral Approach (Mt of CO2)`        / (1e15*`Total Primary Energy Supply (PJ)`)) %>% 
    mutate(`Population (millions)` = 1e6*`Population (millions)`) %>% 
    mutate(`CO2 Sectoral Approach (Mt of CO2)` = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    rename(p = `Population (millions)`) %>% 
    rename(a = `affluence`) %>% 
    rename(e = `energy intensity`) %>% 
    rename(k = `carbon intensity`) %>% 
    rename(F = `CO2 Sectoral Approach (Mt of CO2)`) %>% 
    select(country,year,p,a,e,k,F) %>% 
    filter(year %in% seq(1990,2020,10))
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(country) %>%
    arrange(year) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))/lag(F)*100/10) %>%
    mutate(Cp = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/lag(F)*100/10) %>%
    mutate(Ca = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/lag(F)*100/10) %>%
    mutate(Ce = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/lag(F)*100/10) %>%
    mutate(Ck = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/lag(F)*100/10) %>%
    ungroup() %>% 
    gather(variable,value,-country,-year) %>% 
    filter(year != 1990) %>% 
    mutate(period = factor(year, levels = c(seq(2000,2020,10)), labels=c("1990-2000",
                                                                         "2000-2010",
                                                                         "2010-2020"), ordered=TRUE))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(4, "Set1")
  names(cols)  = c("Cp", "Ca", "Ce", "Ck")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ck"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity"))
  
  #-- Plot ---------------------------------------------------------------------
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(period != "1990-2000", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(period != "1990-2000", variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-2000", variable == "DF"), color="white", fill="white", size=3) +
    geom_point(aes(x=period, y=value), data=iData %>% filter(period != "1990-2000", variable == "DF"))
  
  # Formatting
  p = p +
    colour_scale +
    #ylim(i_ylim) +
    ylab("Change in CO2 emissions [%]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  return(iData)
  
}

library(dplyr)
library(tidyr)
library(zoo)

kd_data <- out_data3 %>% 
  rename(year = TIME) %>% 
  rename(country = COUNTRY) %>% 
  select(country,year,
         `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
         `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
         `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
         `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
         `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
         `Population (millions)`,
         `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
         `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
         `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
         `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`)


special_countries=c('Armenia','Azerbaijan','Belarus',
                    'Bosnia_and_Herzegovina',
                    'Botswana',
                    'Cambodia',
                    'Peoples_Republic_of_China',
                    'Croatia',
                    'Eritrea',
                    'Estonia',
                    'Georgia',
                    'Hong_Kong',
                    'Kazakhstan',
                    'Kosovo',
                    'Kyrgyzstan',
                    'Latvia',
                    'Lithuania',
                    'Macedonia',
                    'Mongolia',
                    'Montenegro',
                    'Namibia',
                    'Russian_Federation',
                    'Serbia',
                    'Tanzania',
                    'Turkmenistan',
                    'Ukraine',
                    'Slovenia',
                    'Uzbekistan',
                    'Former_Soviet_Union',
                    'Former_Yugoslavia',
                    'Iraq',
                    'Mexico',
                    'Kuwait',
                    'Lebanon');   


regular_countries=c('Albania','Algeria','Angola','Argentina',
                    'Austria',
                    'Bahrain',
                    'Bangladesh',
                    'Belgium',
                    'Benin',
                    'Bolivia',
                    'Brunei_Darussalam',
                    'Bulgaria',
                    'Cameroon',
                    'Canada',
                    'Chile',
                    'China',
                    'Chinese_Taipei',
                    'Colombia',
                    'Congo',
                    'Democratic_Republic_of_Congo',
                    'Costa_Rica',
                    'Cuba',
                    'Cyprus',
                    'Czech_Republic',
                    'Denmark',
                    'Dominican_Republic',
                    'Ecuador',
                    'Egypt',
                    'El_Salvador',
                    'Ethiopia',
                    'Finland',
                    'France',
                    'Gabon',
                    'Ghana',
                    'Gibraltar',
                    'Greece',
                    'Guatemala',
                    'Haiti',
                    'Honduras',
                    'Hungary',
                    'Iceland',
                    'India',
                    'Indonesia',
                    'Islamic_Republic_of_Iran',
                    'Ireland',
                    'Israel',
                    'Italy',
                    'Cote_d_Ivoire',
                    'Jamaica',
                    'Japan',
                    'Jordan',
                    'Kenya',
                    'Democratic_Peoples_Republic_of_Korea',
                    'Korea',
                    'Libya',
                    'Luxembourg',
                    'Malaysia',
                    'Malta',
                    'Moldova',
                    'Morocco',
                    'Mozambique',
                    'Myanmar',
                    'Nepal',
                    'Netherlands',
                    'Netherlands_Antilles',
                    'New_Zealand',
                    'Nicaragua',
                    'Nigeria',
                    'Norway',
                    'Oman',
                    'Pakistan',
                    'Panama',
                    'Paraguay',
                    'Peru',
                    'Philippines',
                    'Poland',
                    'Portugal',
                    'Qatar',
                    'Romania',
                    'Saudi_Arabia',
                    'Senegal',
                    'Singapore',
                    'Slovak_Republic',
                    'Spain',
                    'Sri_Lanka',
                    'Sudan',
                    'Sweden',
                    'Switzerland',
                    'Syrian_Arab_Republic',
                    'Tajikistan',
                    'Thailand',
                    'Togo',
                    'Trinidad_and_Tobago',
                    'Tunisia',
                    'Turkey',
                    'United_Arab_Emirates',
                    'United_Kingdom',
                    'Uruguay',
                    'Venezuela',
                    'Yemen',
                    'Zambia',
                    'Zimbabwe',
                    'Vietnam',
                    'Brazil',
                    'South_Africa',
                    'Germany',
                    'USA',
                    'Australia',
                    'World',
                    'FormerSoviet19712010',
                    'FYR19712010',
                    'EIT',
                    'Other_Africa',
                    'Other_Asia',
                    'Other_LAM',
                    'ASIA',
                    'LAM',
                    'OECD90',
                    'MAF',
                    'WORLD_agg',
                    'Ethiopia_Eritrea',
                    'ASIA_WO_CHNIND',
                    'Africa',
                    'SSAWOSA')
jan_countries = c(special_countries, regular_countries)

jan_countries_africa  = gsub("_", " ", gsub("Cote_d_Ivoire", "Cote d'Ivoire", jan_countries[c(5, 9, 21, 24, 37, 43, 47, 53, 54, 62, 64, 67, 68, 82, 86, 89, 94, 95, 102, 115, 120, 126, 128, 135, 136, 139, 147)]))
jan_countries_africa_corrected  = c("Mauritius", "South Sudan", "Niger", gsub("_", " ", gsub("Cote_d_Ivoire", "Cote d'Ivoire", jan_countries[c(5, 9, 21, 24, 36, 37, 43, 47, 53, 54, 62, 64, 67, 68, 82, 86, 89, 94, 95, 102, 115, 120, 126, 128, 135, 136, 139, 147)])))
jan_countries_ssawosa = gsub("_", " ", gsub("Cote_d_Ivoire", "Cote d'Ivoire", jan_countries[c(5, 9, 21, 24, 37, 43, 47, 53, 54,     64, 67, 68, 82, 86,         95, 102, 115, 120, 126,      135, 136,      147)]))
my_countries_ssawosa  = c("South Sudan", "Niger", gsub("_", " ", gsub("Cote_d_Ivoire", "Cote d'Ivoire", c("Angola", "Benin", "Botswana", "Cameroon", "Congo", "Cote_d_Ivoire", "Democratic_Republic_of_Congo", "Ethiopia", "Eritrea",
                                                                                                          "Gabon", "Ghana", "Kenya", "Mozambique", "Namibia", "Nigeria", "Senegal", "Sudan", "Tanzania", "Togo", "Zambia", "Zimbabwe",
                                                                                                          "Other_Africa"))))

jan_countries_all_corrected  = c("Mauritius", "South Sudan", "Niger", gsub("_", " ", gsub("Cote_d_Ivoire", "Cote d'Ivoire", jan_countries)))

top20countries <- get_kayaDecompositionFF_20012015_rel(kd_data %>% filter(country %in% jan_countries_all_corrected),
                                                       startyear = 2005, endyear=2015) %>% 
  arrange(desc(k)) %>% 
  top_n(20, k) %>% 
  bind_rows(
    get_kayaDecompositionFF_20012015_rel(kd_data %>% 
                                           filter(country %in% my_countries_ssawosa) %>% 
                                           gather(variable,value,-year,-country) %>% 
                                           # group_by(country,variable) %>% 
                                           # mutate(value=na.approx(value, na.rm = FALSE)) %>% 
                                           # ungroup() %>% 
                                           group_by(year,variable) %>% 
                                           summarise(value=sum(value, na.rm=TRUE)) %>% 
                                           ungroup() %>% 
                                           spread(variable, value) %>% 
                                           mutate(country="SSA") %>% 
                                           select(country,year,
                                                  `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
                                                  `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
                                                  `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
                                                  `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
                                                  `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
                                                  `Population (millions)`,
                                                  `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
                                                  `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
                                                  `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
                                                  `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), startyear=2005, endyear=2015) %>% 
      mutate(rank=NA)
  )

plot_kayaDecomposition_19702015_rel(kd_data %>% filter(country == "Africa"), i_ylim=c(-10,15))

# plot_kayaDecomposition_19702015_rel(kd_data %>% 
#                                       filter(country %in% jan_countries_africa) %>% 
#                                       gather(variable,value,-year,-country) %>% 
#                                       group_by(country,variable) %>% 
#                                       mutate(value=na.approx(value, na.rm = FALSE)) %>% 
#                                       ungroup() %>% 
#                                       group_by(year,variable) %>% 
#                                       summarise(value=sum(value, na.rm=TRUE)) %>% 
#                                       ungroup() %>% 
#                                       spread(variable, value) %>% 
#                                       mutate(country="Africa (bottom-up)") %>% 
#                                       select(country,year,
#                                              `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
#                                              `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
#                                              `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
#                                              `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
#                                              `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
#                                              `Population (millions)`,
#                                              `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
#                                              `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
#                                              `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
#                                              `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), 
#                                     i_ylim=c(-10,15))

plot_kayaDecomposition_19702015_rel(kd_data %>% 
                                      filter(country %in% jan_countries_africa_corrected) %>% 
                                      gather(variable,value,-year,-country) %>% 
                                      group_by(country,variable) %>% 
                                      mutate(value=na.approx(value, na.rm = FALSE)) %>% 
                                      ungroup() %>% 
                                      group_by(year,variable) %>% 
                                      summarise(value=sum(value, na.rm=TRUE)) %>% 
                                      ungroup() %>% 
                                      spread(variable, value) %>% 
                                      mutate(country="Africa (bottom-up)") %>% 
                                      select(country,year,
                                             `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
                                             `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
                                             `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
                                             `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
                                             `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
                                             `Population (millions)`,
                                             `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
                                             `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
                                             `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
                                             `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), 
                                    i_ylim=c(-10,15),
                                    title = "a) Africa") 
ggsave(filename = "../plots/Figure1a.png", width = 20, height=15, units = "cm")  
# plot_kayaDecomposition_19702015_rel(kd_data %>% 
#                                       filter(country %in% jan_countries_ssawosa) %>% 
#                                       gather(variable,value,-year,-country) %>% 
#                                       group_by(country,variable) %>% 
#                                       mutate(value=na.approx(value, na.rm = FALSE)) %>% 
#                                       ungroup() %>% 
#                                       group_by(year,variable) %>% 
#                                       summarise(value=sum(value, na.rm=TRUE)) %>% 
#                                       ungroup() %>% 
#                                       spread(variable, value) %>% 
#                                       mutate(country="Africa (bottom-up)") %>% 
#                                       select(country,year,
#                                              `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
#                                              `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
#                                              `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
#                                              `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
#                                              `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
#                                              `Population (millions)`,
#                                              `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
#                                              `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
#                                              `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
#                                              `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), 
#                                     i_ylim=c(-10,15))


plot_kayaDecomposition_19702015_rel(kd_data %>% 
                                      filter(country %in% my_countries_ssawosa) %>% 
                                      gather(variable,value,-year,-country) %>% 
                                      # group_by(country,variable) %>% 
                                      # mutate(value=na.approx(value, na.rm = FALSE)) %>% 
                                      # ungroup() %>% 
                                      group_by(year,variable) %>% 
                                      summarise(value=sum(value, na.rm=TRUE)) %>% 
                                      ungroup() %>% 
                                      spread(variable, value) %>% 
                                      mutate(country="Africa (bottom-up)") %>% 
                                      select(country,year,
                                             `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
                                             `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
                                             `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
                                             `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
                                             `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
                                             `Population (millions)`,
                                             `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
                                             `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
                                             `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
                                             `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), 
                                    i_ylim=c(-15,15),
                                    title = "b) Sub-Saharan Africa (w/o South Africa)",
                                    LEGEND=TRUE) 
ggsave(filename = "../plots/Figure1b.png", width = 20, height=15, units = "cm")  

plot_kayaDecompositionFF_19702015_rel(kd_data %>% 
                                        filter(country %in% jan_countries_africa_corrected) %>% 
                                        gather(variable,value,-year,-country) %>% 
                                        group_by(country,variable) %>% 
                                        mutate(value=na.approx(value, na.rm = FALSE)) %>% 
                                        ungroup() %>% 
                                        group_by(year,variable) %>% 
                                        summarise(value=sum(value, na.rm=TRUE)) %>% 
                                        ungroup() %>% 
                                        spread(variable, value) %>% 
                                        mutate(country="Africa (bottom-up)") %>% 
                                        select(country,year,
                                               `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
                                               `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
                                               `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
                                               `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
                                               `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
                                               `Population (millions)`,
                                               `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
                                               `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
                                               `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
                                               `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), 
                                      i_ylim=c(-10,8),
                                      title = "a) Africa") 
ggsave(filename = "../plots/Figure2a.png", width = 20, height=15, units = "cm")  

plot_kayaDecompositionFF_19702015_rel(kd_data %>% 
                                        filter(country %in% my_countries_ssawosa) %>% 
                                        gather(variable,value,-year,-country) %>% 
                                        group_by(country,variable) %>% 
                                        mutate(value=na.approx(value, na.rm = FALSE)) %>% 
                                        ungroup() %>% 
                                        group_by(year,variable) %>% 
                                        summarise(value=sum(value, na.rm=TRUE)) %>% 
                                        ungroup() %>% 
                                        spread(variable, value) %>% 
                                        mutate(country="Africa (bottom-up)") %>% 
                                        select(country,year,
                                               `CO2 Coal and Coal Products`, `CO2 Coal - Announced`, `CO2 Coal - Operating`, `CO2 Coal - Permitted`, `CO2 Coal - Pre-permit development`, `CO2 Coal - Shelved`,
                                               `CO2 Oil `, `CO2 Natural Gas `, `CO2 Other `, `CO2 Sectoral Approach (Mt of CO2)`,
                                               `Coal and Coal Products`, `Coal - Announced`, `Coal - Operating`, `Coal - Permitted`, `Coal - Pre-permit development`, `Coal - Shelved`,
                                               `Oil `,`Natural Gas `,`Other `,`CO2 Reference Approach (Mt of CO2)`,
                                               `Total Primary Energy Supply (PJ)`,`Total Primary Energy Supply (Mtoe)`,`GDP (billion 2000 US$ using exchange rates)`,`GDP (billion 2000 US$ using PPPs)`,
                                               `Population (millions)`,
                                               `PE Coal and Coal Products`, `PE Coal - Announced`, `PE Coal - Operating`, `PE Coal - Permitted`, `PE Coal - Pre-permit development`, `PE Coal - Shelved`,
                                               `PE Peat`,`PE Crude, NGL and Feedstocks`,`PE Petroleum Products`,`PE Natural Gas`,
                                               `PE Nuclear`,`PE Hydro`,`PE Geothermal`,`PE Solar/Wind/Other`,`PE Combustible Renewables and Waste`,
                                               `PE Heat Production from non-specified comb.fuels`,`PE Electricity`,`Heat`,`Total Primary Energy Supply (ktoe)`), 
                                      i_ylim=c(-12,12),
                                      title = "b) Sub-Saharan Africa (w/o South Africa)",
                                      LEGEND=TRUE)
ggsave(filename = "../plots/Figure2b.png", width = 20, height=15, units = "cm")  

test_differences <- kd_data %>% 
  filter(country == "Africa") %>% 
  gather(variable, value, -country, -year) %>% 
  select(year,variable,value) %>% 
  rename(africa=value) %>% 
  left_join(
    kd_data %>% 
      filter(country %in% jan_countries_africa) %>% 
      gather(variable,value,-year,-country) %>% 
      group_by(country,variable) %>% 
      mutate(value=na.approx(value, na.rm = FALSE)) %>% 
      ungroup() %>% 
      group_by(year,variable) %>% 
      summarise(value=sum(value, na.rm=TRUE)) %>% 
      ungroup() %>% 
      rename(africa_bu=value),
    by=c("year","variable")
  ) %>% 
  mutate(diff_abs=africa-africa_bu) %>% 
  mutate(diff_rel=abs(africa-africa_bu)/africa*100)
