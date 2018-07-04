#### DATA ANALYSIS ####
source("Load Data from database.R")

library("lme4")
library("broom")
library("emmeans")


# 1) Does proportion of fertility differ across years?

# fit model
fit <- fertile %>% ungroup() %>% mutate(year = factor(year)) %>% 
  glm(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ year, data = ., family = "quasibinomial")
summary(fit)
anova(fit, test = "F")

# Multiple comparisions
comparisons <- summary(multcomp::glht(fit, multcomp::mcp(year = "Tukey")))
comps <- tidy(cld(comparisons))


output <- augment(fit) %>% 
  select(year, .fitted, .se.fit) %>% 
  as.tibble() %>% 
  mutate(response = plogis(.fitted)) %>% 
  mutate(se2 = logit2prob(output$.se.fit)) %>% 
  mutate(se = plogis(.se.fit)) %>% 
  left_join(comps, by = c("year" = "lhs"))
  

# Backtransform logit
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# make plot for average fitted values plus se/CI for each year
output %>% 
  group_by(year, letters) %>% 
  summarise(response = mean(response), se = mean(se)) %>% 
  ggplot(aes(y = response, x = year, ymin = response -se, ymax = response + se, label = letters)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_text(aes(y = 0.16)) +
  labs(y = "")
  ylim(0.08, 0.17)
  


# GLMER solution
fit <- glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~ factor(year) + (1|species) + (1|blockID), data = fertile, family = "binomial")
summary(fit)








# 2) Does variation in the proportion of fertility differ across the climate grid?
  
  
  
  



# Is the temporal variation in proportion of flowering related to Mean summer Temp, mean temp or ann. prec or previous year climate?

mod01 <- lmer(asin(PropFertile) ~ MeanTemp + MeanSummerTemp + AnnPrec + (1|species) + (1|blockID), fertileC, weights = CountOfspecies)
summary(mod01)
plot(mod01)




# Does proportion flowering differ between PFG, early vs late flowering species and alpine/generalists?
mod01 <- lmer(asin(PropFertile) ~ functionalGroup + (1|species) + (1|blockID), fertileC, weights = CountOfspecies)
summary(mod01)







