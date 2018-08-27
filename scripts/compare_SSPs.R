library(readxl)
library(tidyverse)

get_kayaDecomposition_SSP <- function(iData, i_ylim=c(-140,300), onlyCI=FALSE) {
  
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
  iData = iData  %>%
    filter(variable %in% c("Population", "GDP|PPP", "PE", "Em|CO2|Fossil Fuels and Industry", "PE",
                           "Em|CO2|Coal","Em|CO2|Oil","Em|CO2|Gas","Em|CO2|Other",
                           "PE|Coal","PE|Oil","PE|Gas","PE|Biomass","PE|Nuclear","PE|Hydro",
                           "PE|Geothermal","PE|Wind","PE|Solar","PE|Other")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=Population) %>% 
    mutate(a=(`GDP|PPP`)/(Population)) %>% 
    mutate(e=(`PE`)/(`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(`PE`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    rename(emi_coal  = `Em|CO2|Coal`) %>% 
    rename(emi_oil   = `Em|CO2|Oil`) %>% 
    rename(emi_gas   = `Em|CO2|Gas`) %>% 
    rename(emi_other = `Em|CO2|Other`) %>% 
    rename(pe_coal   = `PE|Coal`) %>% 
    #select(-`PE|Coal`) %>% 
    rename(pe_oil    = `PE|Oil`) %>% 
    #select(-`PE|Oil`) %>% 
    rename(pe_gas    = `PE|Gas`) %>%
    #select(-`PE|Gas`) %>% 
    mutate(pe_other  = ifelse(is.na(`PE|Hydro`),0,`PE|Hydro`) + 
             ifelse(is.na(`PE|Nuclear`),0,`PE|Nuclear`) + 
             ifelse(is.na(`PE|Geothermal`),0,`PE|Geothermal`) + 
             ifelse(is.na(`PE|Solar`),0,`PE|Solar`) + 
             ifelse(is.na(`PE|Wind`),0,`PE|Wind`) + 
             ifelse(is.na(`PE|Biomass`),0,`PE|Biomass`)) %>%  # + `PE|Other`
    select(-`PE|Hydro`, -`PE|Nuclear`, -`PE|Geothermal`, -`PE|Solar`, -`PE|Wind`, -`PE|Biomass`) %>%   #, -`PE|Other`
    mutate(k_coal    = emi_coal /pe_coal) %>% 
    mutate(k_oil     = emi_oil  /pe_oil) %>% 
    mutate(k_gas     = emi_gas  /pe_gas) %>% 
    mutate(k_other   = emi_other/pe_other) %>% 
    rename(pe        = `PE`) %>%
    select(ssp,climate_target,period,p,a,e,k,F,emi_coal,emi_oil,emi_gas,emi_other,pe,pe_coal,pe_oil,pe_gas,pe_other,k_coal,k_oil,k_gas,k_other)
  
  
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
    mutate(Dt = factor(paste0(lag(period),"-",period), ordered=TRUE)) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    mutate(Ckc = Dkc*R) %>% 
    mutate(Cko = Dko*R) %>% 
    mutate(Ckg = Dkg*R) %>% 
    mutate(Ckr = Dkr*R) %>% 
    mutate(rowid=row_number()) %>%
    filter(rowid!=1) %>%
    select(-rowid) %>%
    ungroup() %>% 
    gather(variable,value,-ssp,-climate_target,-period,-Dt) #%>% 
  #filter(period != 2010) %>% 
  #mutate(period = factor(period, levels = c("2020", "2030"), labels=c("2010-2020",
  #                                                                    "2020-2030"), ordered=TRUE))
  
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
  if(onlyCI) {
    p = ggplot()
    p = p +
      geom_bar(aes(x=Dt, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=Dt, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  } else {
    p = ggplot()
    p = p +
      geom_bar(aes(x=Dt, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=Dt, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_point(aes(x=Dt, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      geom_point(aes(x=Dt, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  }
  
  return(list(data=iData, plot=p))
}

get_kayaDecompositionFF_SSP <- function(iData, i_ylim=c(-140,300)) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "PE", "Em|CO2|Fossil Fuels and Industry", "PE",
                           "Em|CO2|Coal","Em|CO2|Oil","Em|CO2|Gas","Em|CO2|Other",
                           "PE|Coal","PE|Oil","PE|Gas","PE|Biomass","PE|Nuclear","PE|Hydro",
                           "PE|Geothermal","PE|Wind","PE|Solar","PE|Other")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=Population) %>% 
    mutate(a=(`GDP|PPP`)/(Population)) %>% 
    mutate(e=(`PE`)/(`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(`PE`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    rename(emi_coal  = `Em|CO2|Coal`) %>% 
    rename(emi_oil   = `Em|CO2|Oil`) %>% 
    rename(emi_gas   = `Em|CO2|Gas`) %>% 
    rename(emi_other = `Em|CO2|Other`) %>% 
    rename(pe_coal   = `PE|Coal`) %>% 
    #select(-`PE|Coal`) %>% 
    rename(pe_oil    = `PE|Oil`) %>% 
    #select(-`PE|Oil`) %>% 
    rename(pe_gas    = `PE|Gas`) %>%
    #select(-`PE|Gas`) %>% 
    mutate(pe_other  = ifelse(is.na(`PE|Hydro`),0,`PE|Hydro`) + 
             ifelse(is.na(`PE|Nuclear`),0,`PE|Nuclear`) + 
             ifelse(is.na(`PE|Geothermal`),0,`PE|Geothermal`) + 
             ifelse(is.na(`PE|Solar`),0,`PE|Solar`) + 
             ifelse(is.na(`PE|Wind`),0,`PE|Wind`) + 
             ifelse(is.na(`PE|Biomass`),0,`PE|Biomass`)) %>%  # + `PE|Other`
    select(-`PE|Hydro`, -`PE|Nuclear`, -`PE|Geothermal`, -`PE|Solar`, -`PE|Wind`, -`PE|Biomass`) %>%   #, -`PE|Other`
    mutate(k_coal    = emi_coal /pe_coal) %>% 
    mutate(k_oil     = emi_oil  /pe_oil) %>% 
    mutate(k_gas     = emi_gas  /pe_gas) %>% 
    mutate(k_other   = emi_other/pe_other) %>% 
    rename(pe        = `PE`) %>%
    select(ssp,climate_target,period,p,a,e,k,F,emi_coal,emi_oil,emi_gas,emi_other,pe,pe_coal,pe_oil,pe_gas,pe_other,k_coal,k_oil,k_gas,k_other)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
    mutate(Dt = factor(paste0(lag(period),"-",period), ordered=TRUE)) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    mutate(Ckc = Dkc*R) %>% 
    mutate(Cko = Dko*R) %>% 
    mutate(Ckg = Dkg*R) %>% 
    mutate(Ckr = Dkr*R) %>%
    mutate(rowid=row_number()) %>%
    filter(rowid!=1) %>%
    select(-rowid) %>%
    ungroup() %>% 
    gather(variable,value,-ssp,-climate_target,-period,-Dt)
  # ungroup() %>% 
  # gather(variable,value,-ssp,-climate_target,-period) %>% 
  # filter(period != 2010) %>% 
  # mutate(period = factor(period, levels = c("2020", "2030"), labels=c("2010-2020",
  #                                                                     "2020-2030"), ordered=TRUE))
  
  print(iData %>% 
          filter(variable %in% c("DF", "Cp","Ca","Ce","Ckc","Cko","Ckg","Ckr", "R")) %>% 
          spread(variable,value))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(7, "Set1")
  cols[4] = "brown" 
  cols[5] = "black"
  cols[6] = "#ff6961"
  cols[7] = "orange"
  names(cols)  = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity (Coal)", "Carbon intensity (Oil)", "Carbon intensity (Gas)", "Carbon intensity (Other)"))
  
  #-- Plot ---------------------------------------------------------------------
  #"Cp","Ca","Ce", 
  #"Cp","Ca","Ce", 
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
    #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
    facet_grid(climate_target~ssp)
  
  # Formatting
  p = p +
    colour_scale +
    #ylim(i_ylim) +
    ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  return(list(data=iData, plot=p))
  
}

plot_kayaDecomposition_SSP <- function(iData, i_ylim=c(-140,300), onlyCI=FALSE) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "Final Energy", "Em|CO2|Fossil Fuels and Industry")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=1e6*Population) %>% 
    mutate(a=(1e9 *`GDP|PPP`)/(1e6*Population)) %>% 
    mutate(e=(1e15*`Final Energy`)/(1e9*`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(1e15*`Final Energy`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    select(ssp,climate_target,period,p,a,e,k,F)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
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
    gather(variable,value,-ssp,-climate_target,-period) %>% 
    filter(period != 2010) %>% 
    mutate(period = factor(period, levels = c("2020", "2030"), labels=c("2010-2020",
                                                                        "2020-2030"), ordered=TRUE))
  
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
  if(onlyCI) {
    p = ggplot()
    p = p +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  } else {
    p = ggplot()
    p = p +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  }
  
  return(iData)
}

plot_kayaDecomposition_SSP_5yr <- function(iData, i_ylim=c(-140,300), onlyCI=FALSE) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "Final Energy", "Em|CO2|Fossil Fuels and Industry")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=1e6*Population) %>% 
    mutate(a=(1e9 *`GDP|PPP`)/(1e6*Population)) %>% 
    mutate(e=(1e15*`Final Energy`)/(1e9*`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(1e15*`Final Energy`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    select(ssp,climate_target,period,p,a,e,k,F)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
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
    gather(variable,value,-ssp,-climate_target,-period) %>% 
    filter(period != 2010) %>% 
    mutate(period = factor(period, levels = c("2015", "2020", "2025", "2030"), labels=c("2010-2015",
                                                                                        "2015-2020",
                                                                                        "2020-2025",
                                                                                        "2025-2030"), ordered=TRUE))
  
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
  if(onlyCI) {
    p = ggplot()
    p = p +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  } else {
    p = ggplot()
    p = p +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  }
  
  return(iData)
}

plot_kayaDecomposition_SSP_rel <- function(iData, i_ylim=c(-140,300), onlyCI=FALSE) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "Final Energy", "Em|CO2|Fossil Fuels and Industry")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=1e6*Population) %>% 
    mutate(a=(1e9 *`GDP|PPP`)/(1e6*Population)) %>% 
    mutate(e=(1e15*`Final Energy`)/(1e9*`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(1e15*`Final Energy`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    select(ssp,climate_target,period,p,a,e,k,F)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
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
    gather(variable,value,-ssp,-climate_target,-period) %>% 
    filter(period != 2010) %>% 
    mutate(period = factor(period, levels = c("2020", "2030"), labels=c("2010-2020",
                                                                        "2020-2030"), ordered=TRUE))
  
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
  if (onlyCI) {
    p = ggplot()
    p = p +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ck")) , stat="identity", position="stack") +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [%]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  } else {
    p = ggplot()
    p = p +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Cp","Ca","Ce","Ck")) , stat="identity", position="stack") +
      geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
      geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
      facet_grid(climate_target~ssp)
    
    # Formatting
    p = p +
      colour_scale +
      #ylim(i_ylim) +
      ylab("Change in CO2 emissions [%]") + xlab("") +
      theme_bw() +
      theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
    print(p) 
  }
  
  return(iData)
}

plot_kayaDecompositionFF_SSP <- function(iData, i_ylim=c(-140,300)) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "Final Energy", "Em|CO2|Fossil Fuels and Industry", "PE",
                           "Em|CO2|Coal","Em|CO2|Oil","Em|CO2|Gas","Em|CO2|Other",
                           "PE|Coal","PE|Oil","PE|Gas","PE|Biomass","PE|Nuclear","PE|Hydro",
                           "PE|Geothermal","PE|Wind","PE|Solar","PE|Other")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=Population) %>% 
    mutate(a=(`GDP|PPP`)/(Population)) %>% 
    mutate(e=(`Final Energy`)/(`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(`Final Energy`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    rename(emi_coal  = `Em|CO2|Coal`) %>% 
    rename(emi_oil   = `Em|CO2|Oil`) %>% 
    rename(emi_gas   = `Em|CO2|Gas`) %>% 
    rename(emi_other = `Em|CO2|Other`) %>% 
    rename(pe_coal   = `PE|Coal`) %>% 
    #select(-`PE|Coal`) %>% 
    rename(pe_oil    = `PE|Oil`) %>% 
    #select(-`PE|Oil`) %>% 
    rename(pe_gas    = `PE|Gas`) %>%
    #select(-`PE|Gas`) %>% 
    mutate(pe_other  = ifelse(is.na(`PE|Hydro`),0,`PE|Hydro`) + 
             ifelse(is.na(`PE|Nuclear`),0,`PE|Nuclear`) + 
             ifelse(is.na(`PE|Geothermal`),0,`PE|Geothermal`) + 
             ifelse(is.na(`PE|Solar`),0,`PE|Solar`) + 
             ifelse(is.na(`PE|Wind`),0,`PE|Wind`) + 
             ifelse(is.na(`PE|Biomass`),0,`PE|Biomass`)) %>%  # + `PE|Other`
    select(-`PE|Hydro`, -`PE|Nuclear`, -`PE|Geothermal`, -`PE|Solar`, -`PE|Wind`, -`PE|Biomass`) %>%   #, -`PE|Other`
    mutate(k_coal    = emi_coal /pe_coal) %>% 
    mutate(k_oil     = emi_oil  /pe_oil) %>% 
    mutate(k_gas     = emi_gas  /pe_gas) %>% 
    mutate(k_other   = emi_other/pe_other) %>% 
    rename(pe        = `PE`) %>%
    select(ssp,climate_target,period,p,a,e,k,F,emi_coal,emi_oil,emi_gas,emi_other,pe,pe_coal,pe_oil,pe_gas,pe_other,k_coal,k_oil,k_gas,k_other)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    mutate(Ckc = Dkc*R) %>% 
    mutate(Cko = Dko*R) %>% 
    mutate(Ckg = Dkg*R) %>% 
    mutate(Ckr = Dkr*R) %>%
    ungroup() %>% 
    gather(variable,value,-ssp,-climate_target,-period) %>% 
    filter(period != 2010) %>% 
    mutate(period = factor(period, levels = c("2020", "2030"), labels=c("2010-2020",
                                                                        "2020-2030"), ordered=TRUE))
  
  print(iData %>% 
          filter(variable %in% c("DF", "Cp","Ca","Ce","Ckc","Cko","Ckg","Ckr", "R")) %>% 
          spread(variable,value))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(7, "Set1")
  cols[4] = "brown" 
  cols[5] = "black"
  cols[6] = "#ff6961"
  cols[7] = "orange"
  names(cols)  = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity (Coal)", "Carbon intensity (Oil)", "Carbon intensity (Gas)", "Carbon intensity (Other)"))
  
  #-- Plot ---------------------------------------------------------------------
  #"Cp","Ca","Ce", 
  #"Cp","Ca","Ce", 
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
    #geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
    facet_grid(climate_target~ssp)
  
  # Formatting
  p = p +
    colour_scale +
    #ylim(i_ylim) +
    ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  return(iData)
  
}

plot_kayaDecompositionFF_SSP_5yr <- function(iData, i_ylim=c(-140,300)) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "Final Energy", "Em|CO2|Fossil Fuels and Industry", "PE",
                           "Em|CO2|Coal","Em|CO2|Oil","Em|CO2|Gas","Em|CO2|Other",
                           "PE|Coal","PE|Oil","PE|Gas","PE|Biomass","PE|Nuclear","PE|Hydro",
                           "PE|Geothermal","PE|Wind","PE|Solar","PE|Other")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=Population) %>% 
    mutate(a=(`GDP|PPP`)/(Population)) %>% 
    #mutate(e=(`Final Energy`)/(`GDP|PPP`)) %>% 
    mutate(e=(`PE`)/(`GDP|PPP`)) %>% 
    #mutate(k=`Em|CO2|Fossil Fuels and Industry`/(`Final Energy`)) %>% 
    mutate(k=(`Em|CO2|Coal`+`Em|CO2|Oil`+`Em|CO2|Gas`+`Em|CO2|Other`)/(`PE`)) %>% 
    mutate(F=(`Em|CO2|Coal`+`Em|CO2|Oil`+`Em|CO2|Gas`+`Em|CO2|Other`)) %>% 
    rename(emi_coal  = `Em|CO2|Coal`) %>% 
    rename(emi_oil   = `Em|CO2|Oil`) %>% 
    rename(emi_gas   = `Em|CO2|Gas`) %>% 
    rename(emi_other = `Em|CO2|Other`) %>% 
    rename(pe_coal   = `PE|Coal`) %>% 
    #select(-`PE|Coal`) %>% 
    rename(pe_oil    = `PE|Oil`) %>% 
    #select(-`PE|Oil`) %>% 
    rename(pe_gas    = `PE|Gas`) %>%
    #select(-`PE|Gas`) %>% 
    mutate(pe_other  = ifelse(is.na(`PE|Hydro`),     0, `PE|Hydro`) + 
             ifelse(is.na(`PE|Nuclear`),   0, `PE|Nuclear`) + 
             ifelse(is.na(`PE|Geothermal`),0, `PE|Geothermal`) + 
             ifelse(is.na(`PE|Solar`),     0, `PE|Solar`) + 
             ifelse(is.na(`PE|Wind`),      0, `PE|Wind`) + 
             ifelse(is.na(`PE|Biomass`),   0, `PE|Biomass`)) %>%  # + `PE|Other`
    select(-`PE|Hydro`, -`PE|Nuclear`, -`PE|Geothermal`, -`PE|Solar`, -`PE|Wind`, -`PE|Biomass`) %>%   #, -`PE|Other`
    mutate(k_coal    = emi_coal /pe_coal) %>% 
    mutate(k_oil     = emi_oil  /pe_oil) %>% 
    mutate(k_gas     = emi_gas  /pe_gas) %>% 
    mutate(k_other   = emi_other/pe_other) %>% 
    rename(pe        = `PE`) %>%
    select(ssp,climate_target,period,p,a,e,k,F,emi_coal,emi_oil,emi_gas,emi_other,pe,pe_coal,pe_oil,pe_gas,pe_other,k_coal,k_oil,k_gas,k_other)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Ratio = (1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De)/R) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)) %>%
    mutate(Ckc = Dkc*R) %>% 
    mutate(Cko = Dko*R) %>% 
    mutate(Ckg = Dkg*R) %>% 
    mutate(Ckr = Dkr*R) %>%
    ungroup() %>% 
    gather(variable,value,-ssp,-climate_target,-period) %>% 
    filter(period != 2010) %>% 
    mutate(period = factor(period, levels = c("2015", "2020", "2025", "2030"), labels=c("2010-2015",
                                                                                        "2015-2020",
                                                                                        "2020-2025",
                                                                                        "2025-2030"), ordered=TRUE))
  
  print(iData %>% 
          filter(variable %in% c("DF", "Cp","Ca","Ce", "Ck","Ckc","Cko","Ckg","Ckr", "R", "Ratio")) %>% 
          spread(variable,value))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  red_shades <- colorRampPalette(5,colors=c("#7E3517", "#E2A76F"))
  
  cols = brewer.pal(7, "Set1")
  
  # cols[1:4] <- red_shades(4)     # Coal Status
  # cols[5]   <- "black"           # Crude oil 
  # cols[6]   <- "#ff7256"         # Natural gas "#0020C2" 
  # cols[7]   <- "#0000ff"         # Other  "#008000"
  
  cols[4] = red_shades(4)[1] #"brown" 
  cols[5] = "black"
  cols[6] = "#6969ff" #"#ff6961"
  cols[7] = "#008000" #"orange"
  
  names(cols)  = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity (Coal)", "Carbon intensity (Oil)", "Carbon intensity (Gas)", "Carbon intensity (Other)"))
  
  #-- Plot ---------------------------------------------------------------------
  #"Cp","Ca","Ce", 
  #"Cp","Ca","Ce", 
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "Ck"), color="white", fill="white", size=3) +
    geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "Ck")) +
    facet_grid(climate_target~ssp)
  
  # Formatting
  p = p +
    colour_scale +
    #ylim(i_ylim) +
    ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  return(iData)
  
}

plot_kayaDecompositionFF_SSP_rel <- function(iData, i_ylim=c(-140,300)) {
  
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
    filter(variable %in% c("Population", "GDP|PPP", "Final Energy", "Em|CO2|Fossil Fuels and Industry", "PE",
                           "Em|CO2|Coal","Em|CO2|Oil","Em|CO2|Gas","Em|CO2|Other",
                           "PE|Coal","PE|Oil","PE|Gas","PE|Biomass","PE|Nuclear","PE|Hydro",
                           "PE|Geothermal","PE|Wind","PE|Solar","PE|Other")) %>% 
    select(-unit) %>% 
    spread(variable, value) %>% 
    mutate(p=Population) %>% 
    mutate(a=(`GDP|PPP`)/(Population)) %>% 
    mutate(e=(`Final Energy`)/(`GDP|PPP`)) %>% 
    mutate(k=`Em|CO2|Fossil Fuels and Industry`/(`Final Energy`)) %>% 
    mutate(F=`Em|CO2|Fossil Fuels and Industry`) %>% 
    rename(emi_coal  = `Em|CO2|Coal`) %>% 
    rename(emi_oil   = `Em|CO2|Oil`) %>% 
    rename(emi_gas   = `Em|CO2|Gas`) %>% 
    rename(emi_other = `Em|CO2|Other`) %>% 
    rename(pe_coal   = `PE|Coal`) %>% 
    #select(-`PE|Coal`) %>% 
    rename(pe_oil    = `PE|Oil`) %>% 
    #select(-`PE|Oil`) %>% 
    rename(pe_gas    = `PE|Gas`) %>%
    #select(-`PE|Gas`) %>% 
    mutate(pe_other  = `PE|Hydro` + `PE|Nuclear` + `PE|Geothermal` + `PE|Solar` + `PE|Wind` + `PE|Biomass`) %>%  # + `PE|Other`
    select(-`PE|Hydro`, -`PE|Nuclear`, -`PE|Geothermal`, -`PE|Solar`, -`PE|Wind`, -`PE|Biomass`) %>%   #, -`PE|Other`
    mutate(k_coal    = emi_coal /pe_coal) %>% 
    mutate(k_oil     = emi_oil  /pe_oil) %>% 
    mutate(k_gas     = emi_gas  /pe_gas) %>% 
    mutate(k_other   = emi_other/pe_other) %>% 
    rename(pe        = `PE`) %>%
    select(ssp,climate_target,period,p,a,e,k,F,emi_coal,emi_oil,emi_gas,emi_other,pe,pe_coal,pe_oil,pe_gas,pe_other,k_coal,k_oil,k_gas,k_other)
  
  #-- Computing Laspeyres decomposition ----------------------------------------
  cat("  > Computing Laspeyres decomposition...\n")
  iData = iData %>%
    group_by(ssp,climate_target) %>%
    arrange(period) %>% 
    mutate(Dp = (p-lag(p))) %>%
    mutate(Da = (a-lag(a))) %>%
    mutate(De = (e-lag(e))) %>%
    mutate(Dk = (k-lag(k))) %>%
    mutate(DF = (F-lag(F))/lag(F)*100/10) %>%
    mutate(Dpecoal  = (pe_coal -lag(pe_coal))) %>%
    mutate(Dpeoil   = (pe_oil  -lag(pe_oil))) %>%
    mutate(Dpegas   = (pe_gas  -lag(pe_gas))) %>%
    mutate(Dpeother = (pe_other-lag(pe_other))) %>%
    mutate(Dkc = 1/pe *(k_coal *pe_coal  - lag(k_coal) *lag(pe_coal)  - Dpecoal *lag(k))) %>% 
    mutate(Dko = 1/pe *(k_oil  *pe_oil   - lag(k_oil)  *lag(pe_oil)   - Dpeoil  *lag(k))) %>% 
    mutate(Dkg = 1/pe *(k_gas  *pe_gas   - lag(k_gas)  *lag(pe_gas)   - Dpegas  *lag(k))) %>% 
    mutate(Dkr = 1/pe *(k_other*pe_other - lag(k_other)*lag(pe_other) - Dpeother*lag(k))) %>%
    mutate(R   = (lag(p)*lag(a)*lag(e)) + 1/2*(Dp*lag(a)*lag(e) + lag(p)*Da*lag(e) + lag(p)*lag(a)*De) + 1/3*(Dp*Da*lag(e) + Dp*lag(a)*De + lag(p)*Da*De) + 1/4*Dp*Da*De) %>%
    mutate(Cp  = laspeyeres(lag(p),lag(a),lag(e),lag(k),Dp,Da,De,Dk)/lag(F)*100/10) %>%
    mutate(Ca  = laspeyeres(lag(a),lag(p),lag(e),lag(k),Da,Dp,De,Dk)/lag(F)*100/10) %>%
    mutate(Ce  = laspeyeres(lag(e),lag(a),lag(p),lag(k),De,Da,Dp,Dk)/lag(F)*100/10) %>%
    mutate(Ck  = laspeyeres(lag(k),lag(a),lag(e),lag(p),Dk,Da,De,Dp)/lag(F)*100/10) %>%
    mutate(Ckc = Dkc*R/lag(F)*100/10) %>% 
    mutate(Cko = Dko*R/lag(F)*100/10) %>% 
    mutate(Ckg = Dkg*R/lag(F)*100/10) %>% 
    mutate(Ckr = Dkr*R/lag(F)*100/10) %>%
    ungroup() %>% 
    gather(variable,value,-ssp,-climate_target,-period) %>% 
    filter(period != 2010) %>% 
    mutate(period = factor(period, levels = c("2020", "2030"), labels=c("2010-2020",
                                                                        "2020-2030"), ordered=TRUE))
  
  print(iData %>% 
          filter(variable %in% c("Cp","Ca","Ce","Ckc","Cko","Ckg","Ckr")) %>% 
          spread(variable,value))
  
  iDatapos = iData %>% mutate(value=ifelse(value >= 0, value, 0))
  iDataneg = iData %>% mutate(value=ifelse(value < 0, value, 0))
  
  
  #== Plot data ================================================================
  cat("  > Plotting data...\n")
  #-- Plotting options ---------------------------------------------------------
  # Colouring options
  require(RColorBrewer)
  require(ggplot2)
  cols = brewer.pal(7, "Set1")
  cols[4] = "brown" 
  cols[5] = "black"
  cols[6] = "#ff6961"
  cols[7] = "orange"
  names(cols)  = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr")
  colour_scale = scale_fill_manual(
    name   = "Kaya factor",
    values = cols,
    breaks = c("Cp", "Ca", "Ce", "Ckc", "Cko", "Ckg", "Ckr"),
    labels = c("Population", "Affluence", "Energy intensity", "Carbon intensity (Coal)", "Carbon intensity (Oil)", "Carbon intensity (Gas)", "Carbon intensity (Other)"))
  
  #-- Plot ---------------------------------------------------------------------
  #"Cp","Ca","Ce", 
  #"Cp","Ca","Ce", 
  p = ggplot()
  p = p +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDatapos %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_bar(aes(x=period, y=value, fill=variable), data=iDataneg %>% filter(variable %in% c("Ckc", "Cko", "Ckg", "Ckr")) , stat="identity", position="stack") +
    geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF"), color="white", fill="white", size=3) +
    geom_point(aes(x=period, y=value), data=iData %>% filter(variable == "DF")) +
    facet_grid(climate_target~ssp)
  
  # Formatting
  p = p +
    colour_scale +
    #ylim(i_ylim) +
    ylab("Change in CO2 emissions [MtCO2]") + xlab("") +
    theme_bw() +
    theme(legend.position="bottom") #, text=element_text(size=18), axis.text.x=element_text(angle=30, vjust=0.9, hjust=1))
  print(p)
  
  return(iData)
  
}

data_ssp1 <- readxl::read_xlsx("SSP1_IMAGE_SSA_data.xlsx")
data_ssp2 <- readxl::read_xlsx("SSP2_MESSAGE_SSA_data.xlsx", sheet = "reformatted")
data_ssp3 <- readxl::read_xlsx("SSP3_AIM_SSA_data.xlsx")
data_ssp4 <- readxl::read_xlsx("SSP4_GCAM_SSA_data.xlsx")
data_ssp5 <- readxl::read_xlsx("SSP5_REMIND_SSA_data.xlsx")

format_data_ssp1 <- function(i_data) {
  
  out <- i_data %>% 
    gather(period, value, -Model, -Scenario, -Region, -Variable, -Unit)
  
  names(out) <- c("model", "scenario", "region", "variable", "unit", "period", "value")
  
  out <- out %>% 
    group_by(model, scenario, variable, unit, period) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "EAF+WAF+RSAF") %>% 
    select(model, scenario, region, variable, unit, period, value)
  
  return(out)
}

format_data_ssp2 <- function(i_data) {
  
  out <- i_data %>% 
    gather(period, value, -Model, -Scenario, -Region, -Variable, -Units)
  
  names(out) <- c("model", "scenario", "region", "variable", "unit", "period", "value")
  
  out <- out %>% 
    mutate(scenario = ifelse(scenario == "SSP2-Ref", "SSP2-Ref-SPA0-V16", "SSP2-26-SPA2-V16"))
  
  return(out)
}

format_data_ssp3 <- function(i_data) {
  
  out <- i_data %>% 
    gather(period, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>% 
    select(MODEL,SCENARIO,REGION,VARIABLE,UNIT,period,value)
  
  names(out) <- c("model", "scenario", "region", "variable", "unit", "period", "value")
  
  return(out)
}

format_data_ssp4 <- function(i_data) {
  
  out <- i_data %>% 
    gather(period, value, -model, -scenario, -region, -Variable, -Unit)
  
  names(out) <- c("model", "scenario", "region", "variable", "unit", "period", "value")
  
  out <- out %>% 
    group_by(model, scenario, variable, unit, period) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "Africa_Eastern+Africa_Western+Africa_Souhtern") %>% 
    select(model, scenario, region, variable, unit, period, value)
  
  return(out)
}

format_data_ssp5 <- function(i_data) {
  
  out <- i_data %>% 
    gather(period, value, -Model, -Scenario, -Region, -Variable, -Unit)
  
  names(out) <- c("model", "scenario", "region", "variable", "unit", "period", "value")
  
  # remove duplicates
  out <- out %>% 
    group_by(model, scenario, region, period) %>% 
    filter(!duplicated(variable)) %>% 
    ungroup()
  
  return(out)
}

data_ssp_markers <- rbind(
  data_ssp1 %>% 
    format_data_ssp1() %>% 
    mutate(variable = ifelse(variable == "Emissions|CO2|Natural Gas", "Emissions|CO2|Gas", variable)) %>%
    rbind(
      data_ssp1 %>% 
        format_data_ssp1() %>%
        filter(variable %in% c("Primary Energy|Coal")) %>% 
        spread(variable, value) %>% 
        mutate(`Emissions|CO2|Other`  = 0) %>% 
        select(-`Primary Energy|Coal`) %>% 
        gather(variable, value, -model,-scenario,-region,-unit,-period) %>% 
        mutate(unit = "Mt CO2/yr")
    ),
  data_ssp2 %>% 
    format_data_ssp2() %>%
    rbind(
      data_ssp2 %>% 
        format_data_ssp2() %>%
        filter(variable %in% c("Emissions|CO2|Coal","Emissions|CO2|Oil","Emissions|CO2|Gas")) %>% 
        spread(variable, value) %>% 
        mutate(`Emissions|CO2|Fossil Fuels and Industry`= `Emissions|CO2|Coal`+`Emissions|CO2|Oil`+`Emissions|CO2|Gas`) %>% 
        select(-`Emissions|CO2|Coal`,-`Emissions|CO2|Oil`,-`Emissions|CO2|Gas`) %>% 
        gather(variable, value, -model,-scenario,-region,-unit,-period) %>% 
        mutate(unit = "Mt CO2/yr")
    ),
  data_ssp3 %>% 
    format_data_ssp3() %>%
    rbind(
      data_ssp3 %>% 
        format_data_ssp3() %>%
        filter(variable %in% c("Primary Energy|Coal", "Primary Energy|Oil", "Primary Energy|Gas", "Emissions|CO2|Fossil Fuels and Industry")) %>% 
        select(-unit) %>% 
        spread(variable, value) %>% 
        mutate(`Emissions|CO2|Total`= `Primary Energy|Coal`*95+`Primary Energy|Oil`*75+`Primary Energy|Gas`*50) %>%
        mutate(`Emissions|CO2|Coal` = `Primary Energy|Coal`*95/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Oil`  = `Primary Energy|Oil`*75/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Gas`  = `Primary Energy|Gas`*50/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        # mutate(`Emissions|CO2|Coal` = `Primary Energy|Coal`*95) %>% 
        # mutate(`Emissions|CO2|Oil`  = `Primary Energy|Oil`*75) %>% 
        # mutate(`Emissions|CO2|Gas`  = `Primary Energy|Gas`*50) %>% 
        mutate(`Emissions|CO2|Other`  = 0) %>% 
        select(-`Primary Energy|Coal`, -`Primary Energy|Oil`, -`Primary Energy|Gas`, -`Emissions|CO2|Fossil Fuels and Industry`,-`Emissions|CO2|Total`) %>% 
        gather(variable, value, -model,-scenario,-region,-period) %>% 
        mutate(unit = "Mt CO2/yr")
    ),
  data_ssp4 %>% 
    format_data_ssp4() %>%
    rbind(
      data_ssp4 %>% 
        format_data_ssp4() %>%
        filter(variable %in% c("Primary Energy|Coal", "Primary Energy|Oil", "Primary Energy|Gas", "Emissions|CO2|Fossil Fuels and Industry")) %>% 
        select(-unit) %>% 
        spread(variable, value) %>% 
        mutate(`Emissions|CO2|Total`= `Primary Energy|Coal`*95+`Primary Energy|Oil`*75+`Primary Energy|Gas`*50) %>%
        mutate(`Emissions|CO2|Coal` = `Primary Energy|Coal`*95/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Oil`  = `Primary Energy|Oil`*75/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Gas`  = `Primary Energy|Gas`*50/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        # mutate(`Emissions|CO2|Coal` = `Primary Energy|Coal`*95) %>% 
        # mutate(`Emissions|CO2|Oil`  = `Primary Energy|Oil`*75) %>% 
        # mutate(`Emissions|CO2|Gas`  = `Primary Energy|Gas`*50) %>% 
        mutate(`Emissions|CO2|Other`  = 0) %>% 
        select(-`Primary Energy|Coal`, -`Primary Energy|Oil`, -`Primary Energy|Gas`, -`Emissions|CO2|Fossil Fuels and Industry`,-`Emissions|CO2|Total`) %>% 
        gather(variable, value, -model,-scenario,-region,-period) %>% 
        mutate(unit = "Mt CO2/yr")
    ),
  data_ssp5 %>% 
    format_data_ssp5() %>%
    rbind(
      data_ssp5 %>% 
        format_data_ssp5() %>%
        filter(variable %in% c("Primary Energy|Coal", "Primary Energy|Oil", "Primary Energy|Gas", "Emissions|CO2|Fossil Fuels and Industry")) %>% 
        select(-unit) %>% 
        spread(variable, value) %>% 
        mutate(`Emissions|CO2|Total`= `Primary Energy|Coal`*95+`Primary Energy|Oil`*75+`Primary Energy|Gas`*50) %>%
        mutate(`Emissions|CO2|Coal` = `Primary Energy|Coal`*95/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Oil`  = `Primary Energy|Oil`*75/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Gas`  = `Primary Energy|Gas`*50/`Emissions|CO2|Total`*`Emissions|CO2|Fossil Fuels and Industry`) %>% 
        mutate(`Emissions|CO2|Other`  = 0) %>% 
        select(-`Primary Energy|Coal`, -`Primary Energy|Oil`, -`Primary Energy|Gas`, -`Emissions|CO2|Fossil Fuels and Industry`,-`Emissions|CO2|Total`) %>% 
        gather(variable, value, -model,-scenario,-region,-period) %>% 
        mutate(unit = "Mt CO2/yr"))) %>% 
  mutate(period = as.numeric(period)) %>% 
  filter(period > 2005, period <= 2030) %>% 
  separate(scenario, into=c("ssp", "climate_target", "spa", "version"), remove = FALSE) %>% 
  mutate(variable = ifelse(grepl("Primary Energy", variable), gsub("Primary Energy", "PE", variable), variable)) %>% 
  mutate(variable = ifelse(grepl("Emissions", variable), gsub("Emissions", "Em", variable), variable)) %>% 
  mutate(climate_target=ifelse(climate_target == "Ref", "Ref", "26 or 34")) %>% 
  mutate(ssp = factor(ssp)) %>% 
  mutate(climate_target = factor(climate_target))

# Interpolate values 
data_ssp_markers <- rbind(data_ssp_markers,
                          data_ssp_markers %>% 
                            filter(period %in% c(2010,2020,2030)) %>% 
                            spread(period, value) %>% 
                            mutate(`2015` = (`2010`+`2020`)/2) %>% 
                            mutate(`2025` = (`2020`+`2030`)/2) %>% 
                            gather(period, value, -model, -scenario, -ssp, -climate_target, -spa, -version, -region, -variable, -unit) %>% 
                            mutate(period=as.numeric(period)) %>% 
                            filter(!period %in% c(2010,2020,2030))) %>% 
  group_by(model,scenario,ssp,climate_target,version,region,variable,unit) %>% 
  arrange(period) %>% 
  ungroup()




kaya_decomposition <- get_kayaDecomposition_SSP(data_ssp_markers %>% filter(period %in% c(2010,2015,2025)))
p <- kaya_decomposition$plot + theme(text=element_text(size=18))
#ggsave(p, filename = "../plots/compare_to_SSPs_20180806.png", width=12, height=8)
#ggsave(p, filename = "../plots/compare_to_SSPs_20180806.svg", width=12, height=8)

ext_kaya_decomposition <- get_kayaDecompositionFF_SSP(data_ssp_markers %>% filter(period %in% c(2015,2025)))
p <- ext_kaya_decomposition$plot +
  scale_y_continuous(breaks = seq(-400,500,100)) +
  theme(text=element_text(size=18))
print(p)
ggsave(p, filename = "../plots/Figure4.png", width=12, height=8)
ggsave(p, filename = "../plots/Figure4.svg", width=12, height=8)



kaya_decomposition$data %>% 
  filter(variable %in% c("Ck", "Ckc")) %>% 
  mutate(sspDt = paste0(ssp, "_", Dt)) %>% 
  select(-period, -ssp, -Dt) %>% 
  spread(sspDt, value) %>%
  write.csv2(file="compare_to_SSPs_data.csv")

plot_kayaDecomposition_SSP(data_ssp_markers)
plot_kayaDecomposition_SSP_5yr(data_ssp_markers)
plot_kayaDecomposition_SSP(data_ssp_markers, onlyCI=T)
plot_kayaDecomposition_SSP_5yr(data_ssp_markers, onlyCI=T)
plot_kayaDecomposition_SSP_rel(data_ssp_markers)
plot_kayaDecomposition_SSP_rel(data_ssp_markers, onlyCI=T)

plot_kayaDecompositionFF_SSP(data_ssp_markers)
plot_kayaDecompositionFF_SSP_5yr(data_ssp_markers)

ggplot(data_ssp_markers %>% 
         filter(variable %in% c("PE|Coal", "PE|Oil", "PE|Gas", "PE|Nuclear", "PE|Wind", "PE|Solar", "PE|Hydro", "PE|Geothermal", "PE|Biomass"))) +
  geom_line(aes(x=period, y=value, color=climate_target), lwd=1) +
  facet_grid(variable~ssp, scales="free_y")

ggplot(data_ssp_markers %>% 
         filter(variable %in% c("Em|CO2", "Em|CO2|Fossil Fuels and Industry", "Em|CO2|Coal", "Em|CO2|Oil", "Em|CO2|Gas"))) +
  geom_line(aes(x=period, y=value, color=climate_target), lwd=1) +
  facet_grid(variable~ssp, scales="free_y")


data_kayadec_ssp_rel <- plot_kayaDecomposition_SSP_rel(data_ssp_markers, onlyCI=T)

ggplot() +
  geom_boxplot(aes(x=1, y=value), data=data_kayadec_ssp_rel %>% filter(variable == "Ck", period == "2010-2020")) +
  geom_jitter(aes(x=1, y=value, colour=ssp), data=data_kayadec_ssp_rel %>% filter(variable == "Ck", period == "2010-2020"), colour="black") +
  geom_boxplot(aes(x=2, y=value), data=data_kayadec_ssp_rel %>% filter(variable == "Ck", period == "2020-2030")) +
  geom_jitter(aes(x=2, y=value, colour=ssp), data=data_kayadec_ssp_rel %>% filter(variable == "Ck", period == "2020-2030"), colour="black") + 
  #geom_jitter(aes(x=1, y=value), data=data_kayadec_ssp_rel %>% filter(variable == "Ck", period == "2010-2020", climate_target == "26 or 34"), colour="red") +
  geom_segment(aes(x=0, xend=2, y=value, yend=value), data=KD_SSA_data %>% filter(variable == "Ck", period == "2010-2020"), colour="red") +
  geom_point(aes(x=0, y=value), data=KD_SSA_data %>% filter(variable == "Ck", period == "2010-2020"), colour="red", size=4) +
  theme_bw()
ggsave(filename = "data_vs_SSPs.pdf", width=10, height=7)
