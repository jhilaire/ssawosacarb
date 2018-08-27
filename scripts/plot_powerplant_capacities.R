# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(remind)
library(luplot)
library(openxlsx)
library(readxl)

red_shades <- colorRampPalette(5,colors=c("#7E3517", "#E2A76F"))
v_alphas        <- seq(0.05,1.0,(1-0.05)/5)
names(v_alphas) <- rev(c("OPR", "CON", "PLN", "DEF", "DEL", "UNK"))
v_fills         <- c(
  #"#800080", # UNK
  "#ff6600", # REN
  "#000080", # HYDRO
  "#ff0000", # UR
  "#00ff00", # BIOMASS
  "#0000ff", # GAS
  "#000000", # OIL
  red_shades(4)[1]) # COAL
names(v_fills) <- rev(c("COAL", "OIL", "GAS", "BIOMASS", "UR", "HYDRO", "REN")) #, "UNK"

# Power plant data
data_pp <- list()
data_pp$coal  = readxl::read_xlsx("data/Global Coal Plant Tracker Feb 2017c.xlsx", sheet = "Projects") %>%
  filter(Region == "Africa and Middle East") %>%
  filter(!Country %in% c("Egypt", "Iran", "Israel", "Jordan", "Morocco", "Oman", "South Africa", "Syria", "United Arab Emirates")) #, "Madagascar", "Mauritius", , "Reunion"
data_pp$coal2  = readxl::read_xlsx("data/Shearer et al 2018 - Coal Africa.xlsx") %>% 
  select(-`All Active Development`) %>% 
  gather(STATUS, CAPACITY, -Country) %>% 
  mutate(FUELCATEGORY = "COAL") %>% 
  rename(COUNTRY=Country) %>% 
  group_by(FUELCATEGORY, STATUS) %>% 
  summarise(CAPACITY=sum(CAPACITY, na.rm=TRUE))

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
  rename(`Pre-Construction`=PLN) %>% 
  mutate(Shelved=DEL+DEF) %>%
  select(-DEL,-DEF) %>% 
  gather(STATUS, CAPACITY, -FUELCATEGORY) %>% 
  filter(!is.na(STATUS), STATUS != "<NA>") #%>% 
#mutate(STATUS=factor(STATUS, levels=c("Operating", "Construction", "Pre-Construction", "Shelved"),
#                     labels=c("Operating", "Construction", "Pre-Construction", "Shelved"), ordered=TRUE))

tmp_data_pp_other2 <- tmp_data_pp_other %>%   
  filter(FUELCATEGORY != "COAL") %>% 
  bind_rows(data_pp$coal2 %>% 
              mutate(CAPACITY = CAPACITY *1e-3)) %>% 
  mutate(STATUS=factor(STATUS, levels=c("Operating", "Construction", "Pre-Construction", "Shelved"),
                       labels=c("Operating", "Construction", "Pre-Construction", "Shelved"), ordered=TRUE)) %>% 
  mutate(FUELCATEGORY = factor(FUELCATEGORY, 
                               levels=c("COAL", "OIL", "GAS", "BIOMASS", "UR", "HYDRO", "REN", "UNK"),
                               ordered=TRUE))

tmp_data_pp_other <- tmp_data_pp_other %>%   
  mutate(STATUS=factor(STATUS, levels=c("Operating", "Construction", "Pre-construction", "Shelved"),
                       labels=c("Operating", "Construction", "Pre-construction", "Shelved"), ordered=TRUE))  

p = ggplot(tmp_data_pp_other2) + 
  geom_bar(aes(x=FUELCATEGORY, y=CAPACITY, fill=FUELCATEGORY), stat="identity", position="stack") +
  facet_wrap(~STATUS, ncol=4) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(0.5,0.75,0.5,0.5), units = "cm"),  
    axis.line  = element_line(size=1.05),
    axis.ticks = element_line(size=1.05),
    axis.title = element_text(colour="black", size = 14),
    axis.text  = element_text(colour="grey", size = 14),
    #axis.text.x = element_text(angle=45, hjust=1, vjust=1),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  #scale_alpha_manual(values = v_alphas, name="Status", labels=c("Unknown", "Delayed (aft. cons.)", "Deferred (bef. cons.)", "Planned", "Construction", "Operational")) +
  scale_fill_manual(values = v_fills, name="Type", labels=rev(c("Oth. Ren.", "Hydro", "Nuclear", "Biomass", "Gas", "Oil", "Coal"))) + 
  xlab("") + ylab("Capacity [GW]")
print(p)
ggsave(p, filename = "../plots/FigureS1.pdf", width = 8, height = 6)
ggsave(p, filename = "../plots/FigureS1.svg", width = 8, height = 6)
ggsave(p, filename = "../plots/FigureS1.png", width = 8, height = 6)