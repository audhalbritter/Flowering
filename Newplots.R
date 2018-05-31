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



### Making a map for each turf for the proportion of flowering per species (occurence 4 years)

# Function to plot points and curves for predicted values and data
plotMaps <- function(dat){
  dat %>% 
    ggplot(aes(x = Year, y = PropFertile, colour = species, linetype = functionalGroup, shape = factor(n))) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(16, 1)) +
    geom_point() +
    geom_line()
}

# Make plots and print PDF
FloweringMaps <- fertile %>% 
  group_by(turfID, species) %>% 
  mutate(n = n()) %>% # count in how many year the species occurs
  filter(n > 2) %>%
  ungroup() %>% 
  group_by(turfID) %>% 
  do(fl.maps = plotMaps(.))

pdf(file = "Output/FloweringMaps.pdf")
FloweringMaps$fl.maps
dev.off()  



### What to test
# Does proportion flowering differ across years?
# Does proportion flowering differ between PFG, early vs late flowering species and alpine/generalists?
# Does proportion of flowering differ across treatments (warm, wet, ww)?


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

