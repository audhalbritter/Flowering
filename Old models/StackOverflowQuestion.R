### StackOverflow question

# Make data frame
dat <- fertile %>% 
  filter(year %in% c("2011", "2012", "2013")) %>% 
  filter(siteID == "Gudmedalen") %>% 
  rowid_to_column(., "ID") %>% 
  select(year, species, blockID, turfID, ID, PropFertile, NumberOfOccurrence) %>% 
  rename(propFlower = PropFertile) %>% 
  ungroup() %>% 
  mutate(year = factor(year)) %>% 
  select(-turfID)


# Want to fit a glmer to understand trend in the proportion of flowering across years. I first fitted a glm to explore and understand the data. Worked well. But when I fit a glmer, same model but with random effects, I get very strange fitted values (after backtranformation to response scale). Compared to the mean of the data they are strange. Should glm and glmer not give similar means? 

# data with year, species, blockID, propFlower, NumberOfOccurrence
dat <- dput(dat)

# Fit glmer model
fit.glmer <- glmer(propFlower ~ year + (1|blockID) + (1|ID), data = dat, family = "binomial", weights = NumberOfOccurrence)

fit.glm <- glm(propFlower ~ year, data = dat, family = "binomial", weights = NumberOfOccurrence)

# fitted values from glmer and glm model
glmer.fitted.values <- augment(fit.glmer) %>% select(year, .fixed) %>%  
  mutate(glmer.fixed = plogis(.fixed)) %>% select(-.fixed) %>% distinct

glm.fitted.values <- augment(fit.glm) %>% select(year, .fitted) %>% 
  mutate(glm.fitted = plogis(.fitted)) %>% select(-.fitted) %>% distinct

# Means
dat %>% 
  group_by(year) %>% 
  summarise(mean = mean(propFlower)) %>% 
  left_join(glm.fitted.values, by = "year") %>% 
  left_join(glmer.fitted.values, by = "year")

