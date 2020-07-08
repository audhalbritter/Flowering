
all <- data.frame(unlist(out3$samples[[1]])) %>% 
  bind_rows(data.frame(unlist(out3$samples[[2]])),
            data.frame(unlist(out3$samples[[3]]))) %>% 
  as.tibble() %>% 
  gather(key = Run, value = Fertile, -c(alpha.1.:beta.12.))


ggplot(aes(y = Fertile, x = alpha.1))


Fertile.pred[i] ~ dbin(p.pred[i] * Inc.pred[i], N.pred[i])
Inc.pred[i] ~ dbern(p.Inc) # zero inflation
logit(p.pred[i]) <- alpha[siteID.pred[i]] + beta[siteID.pred[i]] * Temp.pred[i] + speciesCoeff[species.pred[i]] + eps.pred[i]


summary(out3)

res <- data.frame(unlist(out3$samples[[1]])) %>% 
  bind_rows(data.frame(unlist(out3$samples[[2]])),
            data.frame(unlist(out3$samples[[3]]))) %>% 
  as.tibble() %>% 
  gather(key = ID, value = Value, -c(beta2.1.:deviance)) %>% 
  left_join(sites, by = "ID")

res %>% 
  mutate(temperature_level = plyr::mapvalues(temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(temperature_level = factor(temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  mutate(Pmm = plyr::mapvalues(precipitation_level, c(1, 2, 3, 4), c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  mutate(Pmm = factor(Pmm, levels =  c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  ggplot(aes(x = temperature_level, y = Value, color = temperature_level)) +
  geom_point() +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  facet_grid( ~ Pmm)

load(file = "out3.Rdata")

Dat %>% 
  ggplot(aes(x = MeanSummerTemp, y = PropFertile, color = siteID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ precipitation_level)

Dat %>% 
  ggplot(aes(x = MeanSummerTemp, y = PropFertile, color = factor(year))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ precipitation_level)


Dat$MeanSummerTemp <- Dat$AnnPrec

site <- Dat %>% 
  #filter(precipitation_level == 2) %>% 
  group_by(siteID) %>% 
  summarise(n = n(),
            PropFertile2 = mean(PropFertile, na.rm = TRUE),
            seFl = sd(PropFertile, na.rm = TRUE)/sqrt(n),
            MeanSummerTemp2 = mean(MeanSummerTemp, na.rm = TRUE),
            seT = sd(MeanSummerTemp, na.rm = TRUE)/sqrt(n)) %>% 
  mutate(model = "space")

Dat %>% 
  #filter(precipitation_level == 2) %>% 
  mutate(year = factor(year)) %>% 
  group_by(year, siteID) %>% 
  summarise(n = n(),
            PropFertile2 = mean(PropFertile, na.rm = TRUE),
            seFl = sd(PropFertile, na.rm = TRUE)/sqrt(n),
            MeanSummerTemp2 = mean(MeanSummerTemp, na.rm = TRUE),
            seT = sd(MeanSummerTemp, na.rm = TRUE)/sqrt(n)) %>% 
  #mutate(model = "time") %>% 
  #bind_rows(site) %>% 
  #ggplot(aes(x = MeanSummerTemp, y = PropFertile, colour = model)) +
  ggplot(aes(x = MeanSummerTemp2, xmin = MeanSummerTemp2 - seT, xmax = MeanSummerTemp2 + seT, y = PropFertile2, ymin = PropFertile2 - seFl, ymax = PropFertile2 + seFl, colour = year)) +
  #geom_point(data = Dat, mapping = aes(x = MeanSummerTemp2, y = PropFertile2, colour = "grey50", alpha = 0.3)) +
  geom_point(size = 4) +
  geom_errorbar() +
  geom_errorbarh()
  

fl ~ climateT(grid) + climateP(grid) + WeatherT(Annomalie) + WeatherP(Annomalie) + (1|sp) + (1|year) + (1|site)



# Join climate data with fertil and also join previous year climate data
fertileClimate <- Dat %>% 
  #filter(lifeSpan != "annual") %>% # remove annuals, because previous year climate does not affect them, they have to flower
  mutate(site = substr(siteID, 1, 3)) %>% 
  # Join Climate data from previous year
  mutate(PreviousYear = year - 1) %>% 
  left_join(Climate, by =c("site" = "Site", "PreviousYear" = "Year2")) %>%
  rename(AnnPrec = AnnPrec.x, MeanSummerTemp = MeanSummerTemp.x, PrevAnnPrec = AnnPrec.y, PrevMeanSummerTemp = MeanSummerTemp.y) %>% 
  mutate(SummerTemperatureAverage = case_when(siteID %in% c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen") ~ 6.5,
                                              siteID %in% c("Alrust", "Hogsete", "Rambera", "Veskre") ~ 8.5,
                                              TRUE ~ 10.5)) %>% 
  mutate(PrecipitationAverage = case_when(siteID %in% c("Ulvhaugen", "Alrust", "Fauske") ~ 500,
                                          siteID %in% c("Lavisdalen", "Hogsete", "Vikesland") ~ 1200,
                                          siteID %in% c("Gudmedalen", "Rambera", "Arhelleren") ~ 2000,
                                          TRUE ~ 2700)) %>% 
  # Calculate annomalies
  mutate(AnnPrecAnnomalie = AnnPrec - PrecipitationAverage,
         PrevAnnPrecAnnomalie = PrevAnnPrec - PrecipitationAverage,
         MeanSummerTempAnnomalie = MeanSummerTemp - SummerTemperatureAverage,
         PrevMeanSummerTempAnnomalie = PrevMeanSummerTemp - SummerTemperatureAverage) %>% 
  # centre and scale data
  mutate(AnnPrec.cen = scale(AnnPrec),
         MeanSummerTemp.cen = scale(MeanSummerTemp),
         PrevAnnPrec.cen = scale(PrevAnnPrec),
         PrevMeanSummerTemp.cen = scale(PrevMeanSummerTemp)) %>% 
  mutate(AnnPrecAnnomalie.cen = scale(AnnPrecAnnomalie),
         MeanSummerTempAnnomalie.cen = scale(MeanSummerTempAnnomalie),
         PrevAnnPrecAnnomalie.cen = scale(PrevAnnPrecAnnomalie),
         PrevMeanSummerTempAnnomalie.cen = scale(PrevMeanSummerTempAnnomalie)) 



