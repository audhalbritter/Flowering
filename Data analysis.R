#### DATA ANALYSIS ####
source("Load Data from database.R")

library("lme4")
library("broom")
library("emmeans")


# 1) Does proportion of fertility differ across years?
# GLM
fit.glm <- fertile %>% ungroup() %>% mutate(year = factor(year)) %>% rowid_to_column(., "ID") %>%
  glm(PropFertile ~ year, data = ., family = "quasibinomial")
summary(fit.glm)


# Multiple comparisions
comparisons <- summary(multcomp::glht(fit, multcomp::mcp(year = "Tukey")))
comps <- tidy(cld(comparisons))


# Backtransform logit
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

output <- augment(fit.glm) %>% 
  select(year, .fitted, .se.fit) %>% 
  as.tibble() %>% 
  mutate(response = plogis(.fitted)) %>% 
  mutate(ci.low = plogis(.fitted - 1.96 * .se.fit), ci.high = plogis(.fitted + 1.96 * .se.fit)) %>% 
  left_join(comps, by = c("year" = "lhs"))


# make plot for average fitted values plus se/CI for each year
FertilAcrossYears <- output %>% 
  distinct() %>%  
  ggplot(aes(y = response, x = year, ymin = ci.low, ymax = ci.high, label = letters)) +
  geom_point(size = 4) +
  geom_errorbar(width = 0) +
  geom_text(aes(y = 0.17), size = 6) +
  labs(y = "Proportion of fertile", x = "") +
  ylim(0.08, 0.17)
ggsave(FertilAcrossYears, filename = "Output/FertilAcrossYears.jpeg", dpi = 300, width = 8, height = 5)


# fit GLMER model
# GIVES VERY STRANGE FITTED VALUES, MUCH SMALLER THAN MEANS AND VERY DIFFERENT FROM GLM OUTPUT!!!
fit.glmer <- fertile %>% ungroup() %>% mutate(year = factor(year)) %>% rowid_to_column(., "ID") %>% glmer(PropFertile ~ year + (1|blockID) + (1|ID), data = ., family = "binomial", weights = NumberOfOccurrence)
summary(fit.glmer)
anova(fit.glmer, test = "F")

# glmer can also be fitted with cbind(sucess, nrtrials-sucess)
#glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ year + (1|blockID) + (1|ID), data = ., family = "binomial")

augment(fit.glmer) %>% select(siteID, year, .fixed) %>% 
  mutate(fixed = plogis(.fixed)) %>% distinct %>% pn

fertile %>% 
  group_by(year) %>% 
  summarise(mean = mean(PropFertile))

# get confint
newdat <- expand.grid(
  year = factor(c("2011", "2012", "2013", "2015", "2016", "2017"))
)
newdat$PropFertile <- predict(fit, newdat, re.form = NA)
mm <- model.matrix(terms(fit), newdat)
#newdat$distance <- mm %*% fixef(fit)
pvar1 <- diag(mm %*% tcrossprod(vcov(fit), mm))
tvar1 <- pvar1 + VarCorr(fit)$blockID[1]  ## must be adapted for more complex models
cmult <- 1.96 ## could use 1.96

newdat <- newdat %>% 
  mutate(plo = plogis(PropFertile - cmult*sqrt(pvar1)),
         phi = plogis(PropFertile + cmult*sqrt(pvar1)),
         tlo = plogis(PropFertile - cmult*sqrt(tvar1)),
         thi = plogis(PropFertile + cmult*sqrt(tvar1)),
         PropFertile = plogis(PropFertile))

#plot confidence
g0 <- ggplot(newdat, aes(x = year, y = PropFertile)) + 
  geom_point()
g0 + geom_pointrange(aes(ymin = plo, ymax = phi))





#***********************************************************************************************************
# 2) Does variation in the proportion of fertility differ across the climate grid?
  
# fit model
fit <- fertile %>% ungroup() %>% mutate(year = factor(year), temperature_level = factor(temperature_level), precipitation_level = factor(precipitation_level)) %>% rowid_to_column(., "ID") %>% 
  glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ year + temperature_level * precipitation_level + (1|species) + (1|blockID) + (1|ID), data = ., family = "binomial")
summary(fit)
anova(fit, test = "F")
# add ID from 1-5... as random effect

augment(fit) %>% 
  select(year, temperature_level, precipitation_level, .fitted, .se.fit) %>% 
  mutate(response = plogis(.fitted)) %>% 
  mutate(ci.low = plogis(.fitted - 1.96 * .se.fit), ci.high = plogis(.fitted + 1.96 * .se.fit)) %>% 
  distinct() %>% 
  ggplot(aes(y = response, x = year, ymin = ci.low, ymax = ci.high, group = temperature_level, color = temperature_level)) +
  geom_point(position = position_dodge(width = 0.4)) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  labs(x = "", y = "Proportion fertile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid( ~ precipitation_level)



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








#***********************************************************************************************************
# Does proportion flowering differ between graminoids and forbs

fit <- fertile %>% ungroup() %>% mutate(year = factor(year), temperature_level = factor(temperature_level), precipitation_level = factor(precipitation_level)) %>% 
  glm(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ year * functionalGroup * temperature_level, data = ., family = "quasibinomial")
summary(fit)
anova(fit, test = "F")


augment(fit) %>% 
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







#***********************************************************************************************************
# Is the temporal variation in proportion of flowering related to Mean summer Temp, mean temp or ann. prec or previous year climate?

mod01 <- lmer(asin(PropFertile) ~ MeanTemp + MeanSummerTemp + AnnPrec + (1|species) + (1|blockID), fertileC, weights = CountOfspecies)
summary(mod01)
plot(mod01)
