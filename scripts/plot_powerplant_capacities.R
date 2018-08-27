#################################################################################
## Power plant capacities
#################################################################################
## This script takes in data from PLATTS 2017 and Shearer et 2018 to generate
## operating and planned power plant capacities.
#################################################################################

#== GET DATA ======================================= 
#-- Get Shearer et al 2018 data --------------------
data_pp      <- list()
data_pp$coal <- readxl::read_xlsx("data/Shearer et al 2018 - Coal Africa.xlsx") %>% 
  select(-`All Active Development`) %>% 
  gather(STATUS, CAPACITY, -Country) %>% 
  mutate(FUELCATEGORY = "COAL") %>% 
  rename(COUNTRY=Country) %>% 
  group_by(FUELCATEGORY, STATUS) %>% 
  summarise(CAPACITY=sum(CAPACITY, na.rm=TRUE))

#-- Get PLATTS 2017 data ---------------------------
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


#== PROCESS DATA ===================================
#-- Process PLATTS 2017 data -----------------------
data_figS1 <- data_pp_other %>% 
  mutate(STATUS = toupper(STATUS)) %>% 
  filter(!(STATUS %in% "OPR" & YEAR <= 2015)) %>% 
  filter(FUELCATEGORY != "UNK") %>% 
  group_by(COUNTRY,FUELCATEGORY,STATUS) %>% 
  summarize(CAPACITY=sum(MW)) %>% 
  ungroup() %>% 
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
  rename(`Pre-Construction`=PLN) %>% 
  mutate(Shelved=DEL+DEF) %>%
  select(-DEL,-DEF) %>% 
  gather(STATUS, CAPACITY, -FUELCATEGORY) %>% 
  filter(!is.na(STATUS), STATUS != "<NA>")

#-- Replace coal data from PLATTS 2017 by that from Shearer et al 2018 -----------------------
data_figS1 <- data_figS1 %>%   
  filter(FUELCATEGORY != "COAL") %>% 
  bind_rows(data_pp$coal %>% 
              mutate(CAPACITY = CAPACITY *1e-3)) %>% 
  mutate(STATUS=factor(STATUS, levels=c("Operating", "Construction", "Pre-Construction", "Shelved"),
                       labels=c("Operating", "Construction", "Pre-Construction", "Shelved"), ordered=TRUE)) %>% 
  mutate(FUELCATEGORY = factor(FUELCATEGORY, 
                               levels=c("COAL", "OIL", "GAS", "BIOMASS", "UR", "HYDRO", "REN", "UNK"),
                               ordered=TRUE))


#== PLOT DATA ==================================
#-- Create plot --------------------------------
p = ggplot(data_figS1) + 
  geom_bar(aes(x=FUELCATEGORY, y=CAPACITY, fill=FUELCATEGORY), stat="identity", position="stack") +
  facet_wrap(~STATUS, ncol=4) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.margin     = unit(c(0.5,0.75,0.5,0.5), units = "cm"),  
    axis.line       = element_line(size=1.05),
    axis.ticks      = element_line(size=1.05),
    axis.title      = element_text(colour="black", size = 14),
    axis.text       = element_text(colour="grey", size = 14),
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank()) +
  scale_fill_manual(values = v_fills_ppc, name="Type", labels=rev(c("Oth. Ren.", "Hydro", "Nuclear", "Biomass", "Gas", "Oil", "Coal"))) + 
  xlab("") + ylab("Capacity [GW]")

#-- Show plot -------------------------------- 
print(p)

#-- Save plot --------------------------------
ggsave(p, filename = "plots/FigureS1.pdf", width = 8, height = 6)
ggsave(p, filename = "plots/FigureS1.svg", width = 8, height = 6)
ggsave(p, filename = "plots/FigureS1.png", width = 8, height = 6)

#-- Save plot data ---------------------------
write.csv2(data_figS1, file = "output/data_FigureS1.csv")