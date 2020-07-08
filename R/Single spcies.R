#### SINGLE SPECIES APPROACH ####

### Cam.rot and Ant.odo
# filter species
single <- fertile %>% 
  filter(species %in% c("Cam.rot", "Ant.odo")) %>% 
  group_by(year, species, siteID) %>% mutate(nblock = n()) %>% filter(nblock > 2)

# Plot means across grid
SP <- c(Ant.odo = "Anthoxantum odoratum", Cam.rot = "Campanula rotundifolia")
SingleSP <- single %>% 
  group_by(year, temperature_level, precipitation_level, species) %>% 
  summarise(mean = mean(PropFertile), n = n(), se = sd(PropFertile)/sqrt(n)) %>% 
  mutate(Pmm = plyr::mapvalues(precipitation_level, c(1, 2, 3, 4), c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  mutate(Pmm = factor(Pmm, levels =  c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  ungroup() %>% 
  mutate(temperature_level = plyr::mapvalues(temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(temperature_level = factor(temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  ungroup() %>%
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  mutate(year = year(year)) %>% 
  ggplot(aes(x = year, y = mean, ymin = mean -se, ymax = mean + se, color = factor(temperature_level), group = factor(temperature_level))) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_path() +
  labs(x = "", y = "Proportion of fertile") +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016)) +
  theme(legend.position="top",
        text = element_text(size = 10),
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 10), 
        strip.text.y = element_text(face = "italic")) +
  facet_grid(species ~ Pmm,  labeller=labeller(species = SP))
ggsave(SingleSP, filename = "Output/SingleSP.jpeg", dpi = 300, width = 8, height = 4)



# fit models

# Anthoxantum
fit.ant <- fertile %>% ungroup() %>% mutate(year = factor(year), temperature_level = factor(temperature_level), precipitation_level = factor(precipitation_level)) %>% filter(species == "Ant.odo") %>% 
  glm(PropFertile ~ year + temperature_level * precipitation_level, data = ., family = "binomial", weights = NumberOfOccurrence)
summary(fit.ant)
anova(fit.ant, test = "F")

augment(fit.ant) %>% 
  select(year, temperature_level, precipitation_level, .fitted, .se.fit) %>% 
  mutate(fixed = plogis(.fitted)) %>% distinct


# Campanula
fit.cam <- fertile %>% ungroup() %>% mutate(year = factor(year), temperature_level = factor(temperature_level), precipitation_level = factor(precipitation_level)) %>% filter(species == "Cam.rot") %>% 
  glm(PropFertile ~ year + temperature_level * precipitation_level, data = ., family = "binomial", weights = NumberOfOccurrence)
summary(fit.cam)
anova(fit.cam, test = "F")

augment(fit.cam) %>% 
  select(year, temperature_level, precipitation_level, .fitted, .se.fit) %>% 
  mutate(fixed = plogis(.fitted)) %>% distinct

