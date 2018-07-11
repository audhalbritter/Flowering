#### DATA ANALYSIS ####
# Load libraries
library("lme4")
library("broom")
library("emmeans")

# Load data
source("Load Data from database.R")


## SUBSET DATA
# Select common speciea that occur in 8 or more turfs
commonSP <- fertile %>% 
  filter(NumberOfOccurrence > 20) %>% distinct(species)

fertile <- fertile %>% 
  inner_join(commonSP, by = "species") %>% 
  group_by(year, species) %>% 
  mutate(n = n()) %>% 
  filter(n > 7) %>% 
  ungroup() %>% 
  mutate(year = factor(year), 
         temperature_level = factor(temperature_level), 
         precipitation_level = factor(precipitation_level)) %>% 
  rowid_to_column(., "ID")

# Alternative to write the model: glmer can also be fitted with cbind(sucess, nrtrials-sucess)
#glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ year + (1|species) + (1|blockID), data = ., family = "binomial")

# 1) Does proportion of fertility differ across years?
# GLM
fit.glm <- glm(PropFertile ~ year, data = fertile, family = "quasibinomial")
summary(fit.glm)

# Multiple comparisions
comparisons <- summary(multcomp::glht(fit.glm, multcomp::mcp(year = "Tukey")))
comps <- tidy(cld(comparisons))


# Backtransform logit
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

output.glm <- augment(fit.glm) %>% 
  select(year, .fitted, .se.fit) %>% 
  as.tibble() %>% 
  mutate(response = plogis(.fitted)) %>% 
  mutate(ci.low = plogis(.fitted - 1.96 * .se.fit), ci.high = plogis(.fitted + 1.96 * .se.fit)) %>% 
  left_join(comps, by = c("year" = "lhs"))


# make plot for average fitted values plus se/CI for each year
FertilAcrossYears <- output.glm %>% 
  mutate(year = as.Date(as.character(year), format = "%Y")) %>% 
  distinct() %>%  
  ggplot(aes(y = response, x = year, ymin = ci.low, ymax = ci.high, label = letters)) +
  geom_point(size = 4) +
  geom_errorbar(width = 0) +
  geom_text(aes(y = 0.17), size = 6) +
  labs(y = "Proportion of fertile", x = "")
ggsave(FertilAcrossYears, filename = "Output/FertilAcrossYears.jpeg", dpi = 300, width = 8, height = 5)


# fit GLMER model
fit.glmer <- glmer(PropFertile ~ year + (1|species) + (1|blockID), data = fertile, family = "binomial", weights = NumberOfOccurrence)
summary(fit.glmer)

# Posthoc Tueky test
glmer.comp <- summary(multcomp::glht(fit.glmer, multcomp::mcp(year = "Tukey")))
glmer.comp2 <- tidy(cld(glmer.comp))

# compare means and fitted values
augment(fit.glmer) %>% select(year, .fixed) %>% 
  mutate(fixed = plogis(.fixed)) %>% distinct

fertile %>% 
  group_by(year) %>% 
  summarise(mean = mean(PropFertile))

# get confint
newdat <- expand.grid(
  year = factor(c("2009", "2011", "2012", "2013", "2015", "2016", "2017"))
)
newdat$PropFertile <- predict(fit.glmer, newdat, re.form = NA)
mm <- model.matrix(terms(fit.glmer), newdat)
#newdat$distance <- mm %*% fixef(fit)
pvar1 <- diag(mm %*% tcrossprod(vcov(fit.glmer), mm))
tvar1 <- pvar1 + VarCorr(fit.glmer)$blockID[1] + VarCorr(fit.glmer)$species[1]## must be adapted for more complex models
cmult <- 1.96 ## could use 1.96

newdat <- newdat %>% 
  mutate(plo = plogis(PropFertile - cmult*sqrt(pvar1)),
         phi = plogis(PropFertile + cmult*sqrt(pvar1)),
         tlo = plogis(PropFertile - cmult*sqrt(tvar1)),
         thi = plogis(PropFertile + cmult*sqrt(tvar1)),
         PropFertile = plogis(PropFertile)) %>% 
  left_join(glmer.comp2, by = c("year" = "lhs"))

#plot confidence
FertilAcrossYearsGLMER <- ggplot(newdat, aes(x = year, y = PropFertile, label = letters)) + 
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = plo, ymax = phi)) +
  geom_text(aes(y = 0.17), size = 6) +
  labs(y = "Proportion of fertile", x = "")
ggsave(FertilAcrossYearsGLMER, filename = "Output/FertilAcrossYearsGLMER.jpeg", dpi = 300, width = 8, height = 5)


#***********************************************************************************************************
# 2) Does variation in the proportion of fertility differ across the climate grid?
  
# fit GLM model
fit.glm2 <- glm(PropFertile ~ year + temperature_level * precipitation_level, data = fertile, family = "quasibinomial")
summary(fit.glm2)
anova(fit.glm2, test = "F")

output.glm2 <- augment(fit.glm2) %>% 
  select(year, temperature_level, precipitation_level, .fitted, .se.fit) %>% 
  group_by(temperature_level, precipitation_level) %>% 
  summarise(.fitted = mean(.fitted), .se.fit = mean(.se.fit)) %>% 
  mutate(response = plogis(.fitted)) %>% 
  mutate(ci.low = plogis(.fitted - 1.96 * .se.fit), ci.high = plogis(.fitted + 1.96 * .se.fit)) %>% distinct


# make plot for average fitted values plus se/CI for each year
FertilAcrossGrid <- output.glm2 %>% 
  #mutate(year = as.Date(year, format = "%Y")) %>% 
  mutate(Pmm = plyr::mapvalues(precipitation_level, c(1, 2, 3, 4), c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  mutate(Pmm = factor(Pmm, levels =  c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  ungroup() %>% 
  mutate(temperature_level = plyr::mapvalues(temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(temperature_level = factor(temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  ggplot(aes(y = response, x = Pmm, ymin = ci.low, ymax = ci.high, colour = temperature_level)) +
  geom_point(size = 2, position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  labs(y = "Proportion of fertile", x = "")
ggsave(FertilAcrossGrid, filename = "Output/FertilAcrossGrid.jpeg", dpi = 300, width = 8, height = 5)



# fit GLMER model
fit.glmer2 <- glmer(PropFertile ~ year + temperature_level * precipitation_level + (1|species), data = fertile, family = "binomial", weights = NumberOfOccurrence)
# model is still overdispersed
summary(fit.glmer2)


# get confint
newdat2 <- expand.grid(
  year = factor(c("2009", "2011", "2012", "2013", "2015", "2016", "2017")),
  temperature_level = factor(c(1, 2, 3)),
  precipitation_level = factor(c(1, 2, 3, 4))
)
newdat2$PropFertile <- predict(fit.glmer2, newdat2, re.form = NA)
mm <- model.matrix(terms(fit.glmer2), newdat2)
pvar1 <- diag(mm %*% tcrossprod(vcov(fit.glmer2), mm))
cmult <- 1.96 ## could use 1.96

newdat2 <- newdat2 %>% 
  mutate(plo = plogis(PropFertile - cmult*sqrt(pvar1)),
         phi = plogis(PropFertile + cmult*sqrt(pvar1)),
         PropFertile = plogis(PropFertile)) %>% 
  group_by(temperature_level, precipitation_level) %>% 
  summarise(PropFertile = mean(PropFertile), plo = mean(plo), phi = mean(phi))

#plot confidence
FertilAcrossGridGLMER <- ggplot(newdat2, aes(x = precipitation_level, y = PropFertile, color = temperature_level)) + 
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_pointrange(aes(ymin = plo, ymax = phi), position = position_dodge(width = 0.3)) +
  scale_color_manual(name = "Temperature level", labels = c("alpine", "subalpine", "boreal"), values = c("#56B4E9", "#E69F00", "#D55E00")) +
  scale_x_discrete(breaks = c("1","2","3","4"), labels = c("500mm", "1200mm", "2000mm", "2700mm")) +
  labs(y = "Proportion of fertile", x = "")
ggsave(FertilAcrossGridGLMER, filename = "Output/FertilAcrossGridGLMER.jpeg", dpi = 300, width = 8, height = 5)



# Plot raw data proportion flowering across grid
fertile %>% 
  group_by(temperature_level, precipitation_level) %>% 
  summarise(mean = mean(PropFertile), n = n(), se = sd(PropFertile)/sqrt(n)) %>% 
  ggplot(aes(x = precipitation_level, y = mean, ymin = mean - se, ymax = mean + se, color = factor(temperature_level))) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_line() +
  #geom_smooth(method = 'loess', formula = y ~ x) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  labs(x = "", y = "Proportion fertile")





#***********************************************************************************************************
# Does proportion flowering differ between graminoids and forbs

fit.glm.fG <- fertile %>% ungroup() %>% mutate(year = factor(year), temperature_level = factor(temperature_level), precipitation_level = factor(precipitation_level)) %>% 
  glm(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ year * functionalGroup * temperature_level, data = ., family = "quasibinomial")
summary(fit.glm.fG)
anova(fit.glm.fG, test = "F")


ForbsVsGrass <- augment(fit.glm.fG) %>% 
  select(year, temperature_level, functionalGroup, .fitted, .se.fit) %>% 
  mutate(response = plogis(.fitted)) %>% 
  mutate(ci.low = plogis(.fitted - 1.96 * .se.fit), ci.high = plogis(.fitted + 1.96 * .se.fit)) %>% 
  distinct() %>% 
  ggplot(aes(y = response, x = year, ymin = ci.low, ymax = ci.high, group = temperature_level, color = temperature_level)) +
  geom_point(position = position_dodge(width = 0.4)) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  labs(x = "", y = "Proportion fertile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid( ~ functionalGroup)
ggsave(ForbsVsGrass, filename = "Output/ForbsVsGrass.jpeg", dpi = 300, width = 8, height = 5)






#***********************************************************************************************************
# 3) Is the temporal variation in proportion of flowering related to Mean summer Temp, mean temp or ann. prec or previous year climate?


ClimatePlot <- monthlyClimate %>% 
  filter(Logger == "Temperature" & month(Date) %in% 6:9 | Logger == "Precipitation") %>%
  mutate(Year = year(Date)) %>% 
  group_by(Year, Site, Logger) %>%
  summarise(n = n(), mean = mean(Value), sum = sum(Value)) %>% 
  mutate(Value = ifelse(Logger == "Precipitation", sum, mean)) %>% 
  select(-n, -sum, -mean) %>% 
  spread(key = Logger, value = Value) %>% 
  mutate(Temperature_level = case_when(Site %in% c("Ulv", "Lav", "Gud", "Skj") ~ "alpine",
                                       Site %in% c("Alr", "Hog", "Ram", "Ves") ~ "subalpine",
                                       TRUE ~ "boreal")) %>% 
  mutate(Temperature_level = factor(Temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  mutate(Precipitation_level = case_when(Site %in% c("Ulv", "Alr", "Fau") ~ "500mm",
                                         Site %in% c("Lav", "Hog", "Vik") ~ "1200mm",
                                         Site %in% c("Gud", "Ram", "Arh") ~ "2000mm",
                                         TRUE ~ "2700mm")) %>% 
  mutate(Precipitation_level = factor(Precipitation_level, levels =  c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  ggplot(aes(x = Temperature, y = Precipitation, colour = factor(Year))) +
  geom_point() +
  facet_grid(Temperature_level ~ Precipitation_level) +
  guides(color = guide_legend(title = "Year")) +
  theme_bw()
ggsave(ClimatePlot, filename = "Output/ClimatePlot.jpeg", dpi = 300, width = 8, height = 5)



library("MuMIn")
options(na.action = "na.fail") # can also be put in the model
options(na.action = "na.omit")

# Join climate data with fertil and also join previous year climate data
fertileClimate <- fertile %>% 
  mutate(site = substr(siteID, 1, 3)) %>% 
  # Join Climate data
  left_join(Climate, by =c("site" = "Site", "year" = "Year")) %>%
  # Join Climate data from previous year
  mutate(PreviousYear = year - 1) %>% 
  left_join(Climate, by =c("site" = "Site", "PreviousYear" = "Year")) %>%
  rename(AnnPrec = AnnPrec.x, MeanSummerTemp = MeanSummerTemp.x, MeanTemp = MeanTemp.x, PrevAnnPrec = AnnPrec.y, PrevMeanSummerTemp = MeanSummerTemp.y, PrevMeanTemp = MeanTemp.y)


# fit GLM model with all climate variables (need to be this version of the model)
modClimate <- glm(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ AnnPrec + MeanSummerTemp + MeanTemp + PrevAnnPrec + PrevMeanSummerTemp + PrevMeanTemp, data = fertileClimate, family = "binomial")
summary(modClimate)

percent.thresh <- 0.95
model.set <- dredge(modClimate, rank = "AICc", extra = "R^2")
mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm95 <- mm %>% filter(cumsum < percent.thresh)

model.avg(model.set, subset = cumsum(weight) <= percent.thresh)

averaged.model <- model.avg(model.set, subset = cumsum(weight) <= percent.thresh)
res <- data.frame(summary(averaged.model)$coefmat.full)
res

imp <- data.frame(importance(averaged.model))
imp


res %>% 
  rownames_to_column(var = "Variable") %>% 
  setNames(., c("Variable", "Estimate", "StError", "AdjSE", "Zvalue", "Pvalue")) %>% 
  #mutate(Intercept = first(Estimate), StErrorInercept = first(StError)) %>% 
  #filter(!Variable == "(Intercept)") %>% 
  #mutate(Estimate = Estimate + Intercept, StError = StError + StErrorInercept) %>% 
  # Backtransform to response scale
  mutate(CI.low = plogis(Estimate - 1.96 * StError), CI.high = plogis(Estimate + 1.96 * StError)) %>% 
  mutate(Estimate = plogis(Estimate)) %>% 
  ggplot(aes(x = Variable, y = Estimate, ymin = CI.low, ymax = CI.high)) + 
  geom_point() +
  labs(x = "") +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(width=0.25) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


fertileClimate %>% 
  gather(key = Climate, value = value, AnnPrec, MeanSummerTemp, MeanTemp, PrevAnnPrec, PrevMeanSummerTemp, PrevMeanTemp) %>% 
  ggplot(aes(x = value, y = PropFertile, color = factor(year))) +
  geom_point() +
  facet_wrap(~ Climate, scales = "free_x")


