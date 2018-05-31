library("readxl")
library("tidyverse")
library("ggplot2")
library("cowplot")

# Colours
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Import data
fertile <- read_excel(path = "fertility3.xlsx")

fertile <- fertile %>% 
  mutate(SumOffertile = SumOffertile * -1) %>% 
  mutate(PropFertile = SumOffertile / CountOfspecies) %>% # calculate proportion fertile
  mutate(CoverFertile = SumOffertile / cover) %>% 
  mutate(biomass = cover * vegetationHeight) %>% # biomass
  mutate(Temperature_level = plyr::mapvalues(Temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(Temperature_level = factor(Temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("TTC", "TT1", "TT2", "TT3", "TT4"), c("Control", "Control", "Warm", "Wet", "WarmWet"))) %>% 
  mutate(newTT = factor(newTT, c("Control", "Warm", "Wet", "WarmWet")))



### Maps


  
ggplot(plotdata, aes(x = Year, y = PropFertile, color = newTT)) +
  geom_smooth(method = 'loess', se = FALSE, formula = y ~ x) +
  scale_color_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#CC79A7")) +
  labs(x = "", y = "Proportion fertile") +
  facet_grid(functionalGroup ~ Temperature_level)



plotdata <- fertile %>% 
  group_by(siteID, Temperature_level, Precipitation_level, TTtreat, Year, functionalGroup) %>% 
  #summarise(n = n(), mean = mean(PropFertile)) %>% 
  filter(TTtreat %in% c("TTC"))

ggplot(plotdata, aes(x = Year, y = PropFertile, color = factor(Temperature_level))) +
  geom_smooth(method = 'loess', formula = y ~ x) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
  labs(x = "", y = "Proportion fertile") +
  facet_grid(functionalGroup ~ Precipitation_level)

