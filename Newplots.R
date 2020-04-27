library("readxl")
library("tidyverse")
library("lubridate")
library("cowplot")



# Colours
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Import data
fertile <- read_excel(path = "fertility3.xlsx")

fertile <- fertile %>% 
  rename(FVar = `F-Vår`, FFso = `F-Fso`, FMso = `F-Mso`, FSso = `F-Sso`, FHo = `F-Hø`) %>% 
  mutate(SumOffertile = SumOffertile * -1) %>% 
  mutate(PropFertile = SumOffertile / CountOfspecies) %>% # calculate proportion fertile
  mutate(CoverFertile = SumOffertile / cover) %>% 
  mutate(biomass = cover * vegetationHeight) %>% # biomass
  mutate(Temperature_level = plyr::mapvalues(Temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(Temperature_level = factor(Temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("TTC", "TT1", "TT2", "TT3", "TT4"), c("Control", "Control", "Warm", "Wet", "WarmWet"))) %>% 
  mutate(newTT = factor(newTT, c("Control", "Warm", "Wet", "WarmWet"))) %>% 
  # TRAIT DATA
  mutate(FVar = ifelse(FVar == "NA", 0, FVar),
         FFso = ifelse(FFso == "NA", 0, FFso),
         FVar = ifelse(FMso == "NA", 0, FMso),
         FMso = ifelse(FVar == "NA", 0, FVar),
         FHo = ifelse(FHo == "NA", 0, FHo),
         Nem = ifelse(Nem == "NA", 0, Nem),
         BNem = ifelse(BNem == "NA", 0, BNem),
         SBor = ifelse(SBor == "NA", 0, SBor),
         MBor = ifelse(MBor == "NA", 0, MBor),
         Nbor = ifelse(Nbor == "NA", 0, Nbor),
         LAlp = ifelse(LAlp == "NA", 0, LAlp),
         MAlp = ifelse(MAlp == "NA", 0, MAlp),
         HAlp = ifelse(HAlp == "NA", 0, HAlp)) %>% 
  #mutate(occurrence = ifelse(Nem == 0 & BNem == 0 & SBor == 0 & LAlp == 1, "alpine",
                             #ifelse(HAlp == 0 & MAlp == 0 & LAlp == 0 & Nem == 1, "lowland", "general"))) 
  mutate(occurrence = case_when(Nem == 0 & BNem == 0 & SBor == 0 & LAlp == 1 ~ "alpine",
                                HAlp == 0 & MAlp == 0 & LAlp == 0 & Nem == 1 ~ "lowland",
                                TRUE ~ "general")) %>%
  mutate(flTime = case_when(FMso == 0 & FSso == 0 & FHo == 0 ~ "early",
                            FVar == 0 & FFso == 0 & FMso == 0 ~ "late",
                            TRUE ~ "none"))

### Making a map for each turf for the proportion of flowering per species (occurence 4 years)

# Function to plot points and curves for predicted values and data
plotMaps <- function(dat){
  dat %>% 
    ggplot(aes(x = Year, y = PropFertile, colour = species, linetype = functionalGroup, shape = factor(n))) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(1, 16)) +
    ggtitle(unique(dat$turfID)) +
    labs(x = "", y = "Proportion flowering") +
    geom_point() +
    geom_line()
}

### Make plots and print PDF
FloweringMaps <- fertile %>% 
  group_by(turfID, species) %>% 
  mutate(n = n()) %>% # count in how many year the species occurs
  filter(n > 2) %>% # BUT I WANT 3 CONSECUTIVE YEARS!!! NEEDS FIXING
  ungroup() %>% 
  group_by(turfID) %>% 
  do(fl.maps = plotMaps(.))

pdf(file = "Output/FloweringMaps.pdf")
FloweringMaps$fl.maps
dev.off()  


dd <- fertile %>% 
  filter(turfID == "5 TTC") %>% 
  group_by(turfID, species) %>% 
  mutate(n = n()) %>% # count in how many year the species occurs
  filter(n > 2)

ggplot(dd, aes(x = Year, y = PropFertile, colour = species, linetype = functionalGroup, shape = factor(n))) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_shape_manual(values = c(1, 16)) +
  #ggtitle(label = unique(turfID)) +
  labs(x = "", y = "Proportion flowering") +
  geom_point() +
  geom_line()






# Load Climate data
load("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Monthly.Temperature_2008-2017.RData", verbose=TRUE)
MeanTemp <- monthlyTemperature %>% 
  mutate(year = year(date)) %>% 
  filter(logger == "temp30cm", year > 2009) %>% 
  group_by(site, year) %>% 
  summarise(MeanTemp = mean(value, na.rm = TRUE))
  
MeanSummerTemp <- monthlyTemperature %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(logger == "temp30cm", year > 2009) %>% 
  filter(month %in% c(6, 7, 8, 9)) %>% 
  group_by(site, year) %>% 
  summarise(MeanSummerTemp = mean(value, na.rm = TRUE))


### GRIDDED DATA
load("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedMonth_AnnualClimate2009-2017.Rdata", verbose=TRUE)
annual <- monthlyClimate %>% 
  filter(Logger %in% c("Precipitation", "Temperature")) %>% 
  mutate(year = year(dateMonth)) %>% 
  spread(key = Logger, value = value) %>% 
  group_by(Site, year) %>% 
  summarise(AnnPrec = sum(Precipitation, na.rm = TRUE), MeanTemp = mean(Temperature, na.rm = TRUE)) %>% 
  rename(site = Site) %>% 
  gather(key = variable, value = value, MeanTemp, AnnPrec)

summer <- monthlyClimate %>% 
  filter(Logger %in% c("Temperature")) %>% 
  mutate(year = year(dateMonth), month = month(dateMonth)) %>%
  filter(month %in% c(6, 7, 8, 9)) %>% 
  group_by(Site, year) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  rename(site = Site) %>% 
  mutate(variable = "MeanSummerTemp")


Climate <- annual %>% 
  rbind(summer) %>% 
  spread(key = variable, value = value) %>% 
  mutate(Temperature_level = case_when(site %in% c("Ulv", "Lav", "Gud", "Skj") ~ "alpine",
                                       site %in% c("Alr", "Hog", "Ram", "Ves") ~ "subalpine",
                                       TRUE ~ "boreal")) %>% 
  mutate(Temperature_level = factor(Temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  mutate(Precipitation_level = case_when(site %in% c("Ulv", "Alr", "Fau") ~ 1,
                                         site %in% c("Lav", "Hog", "Vik") ~ 2,
                                         site %in% c("Gud", "Ram", "Arh") ~ 3,
                                         TRUE ~ 4))
  

# Control plots, combine with climate data
fertileC <- fertile %>% 
  filter(TTtreat %in% c("TTC", "TT1")) %>% 
  mutate(site = substr(siteID, 1, 3)) %>% 
  # Join Climate data
  left_join(Climate, by =c("site" = "site", "Year" = "year")) %>%
  # Join Climate data from previous year
  mutate(PreviousYear = Year - 1) %>% 
  left_join(Climate, by =c("site" = "site", "PreviousYear" = "year")) %>%
  rename(AnnPrec = AnnPrec.x, MeanSummerTemp = MeanSummerTemp.x, MeanTemp = MeanTemp.x, PrevAnnPrec = AnnPrec.y, PrevMeanSummerTemp = MeanSummerTemp.y, PrevMeanTemp = MeanTemp.y) %>% 
  # remove first year
  filter(Year > 2009) %>% 
  mutate(Year = as.Date(as.character(Year), format = "%Y")) %>% 
  mutate(logPropFertile = log(PropFertile) * (-1)) %>% 
  # remove species that occur in less than 3 years (needs to be changed!!!)
  group_by(turfID, species) %>% 
  mutate(nYears = n()) %>%
  filter(nYears < 3) %>% 
  # remove species that only occur in less than 3 subplots, otherwise 0 or 100% flowering
  ungroup() %>% 
  group_by(siteID, species, Year) %>% 
  mutate(nSubPlots = n()) %>% 
  filter(nSubPlots > 1)


# Proportion flowering over time
fertile %>% 
  group_by(year, temperature_level, precipitation_level) %>% 
  summarise(mean = mean(PropFertile), n = n(), se = sd(PropFertile)/sqrt(n)) %>% 
  ggplot(aes(x = year, y = mean, ymin = mean - se, ymax = mean + se, color = factor(temperature_level))) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_line() +
  #geom_smooth(method = 'loess', formula = y ~ x) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
  labs(x = "", y = "Proportion fertile") +
  #scale_x_date(date_labels =  "%Y") +
  facet_grid(temperature_level ~ precipitation_level)



# Climate for the different years
# are there cold an warm years?
ggplot(Climate, aes(x = AnnPrec*10, y = MeanSummerTemp, color = factor(year))) +
  geom_point() +
  facet_grid(Temperature_level ~ Precipitation_level)

ggplot(fertileC, aes(x = MeanSummerTemp, y = PropFertile)) +
  geom_point(aes(color = factor(Year))) +
  geom_smooth() +
  facet_grid(Temperature_level ~ Precipitation_level)


ggplot(fertileC, aes(x = MeanSummerTemp, y = PropFertile, color = Temperature_level)) +
  geom_point(aes(shape = factor(Year))) +
  geom_smooth(method = "lm") +
  facet_grid(functionalGroup ~ Precipitation_level)





### Treatments
ggplot(fertile, aes(x = Year, y = PropFertile, color = newTT)) +
  geom_smooth(method = 'loess', formula = y ~ x) +
  scale_color_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#CC79A7")) +
  labs(x = "", y = "Proportion fertile") +
  facet_grid(functionalGroup ~ Temperature_level)




