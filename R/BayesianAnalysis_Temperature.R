##############################
   ### Baysian Analysis ###
##############################

# logit-link function and inverse
logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

# Data (use small data set for now!!!)
# Need to exclude 2009 from analysis, because we do not have climate data from 2008. I should get this data!!!
Dat <- fertile %>% 
  filter(!year %in% c(2009))

# glmer and check for overdispersion
# fit <- glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile) ~
#                MeanSummerTempGrid.sc +
#                MeanSummerTempAnnomalie.sc +
#                AnnPrecGrid.sc +
#                AnnPrecAnnomalie.sc +
#                MeanSummerTempPrevAnnomalie.sc +
#                AnnPrecPrevAnnomalie.sc +
#                MeanSummerTempGrid.sc:AnnPrecGrid.sc +
#                # MeanSummerTempGrid.sc:AnnPrecAnnomalie.sc +
#                # AnnPrecGrid.sc:MeanSummerTempAnnomalie.sc +
#                (1|species) + (1|siteID) + (1|year), data = Dat, family = "binomial")
# summary(fit)
# sim_fmp <- simulateResiduals(fit, refit=T)
# testOverdispersion(sim_fmp)


Data = list(N = Dat$NumberOfOccurrence,
            Fertile = Dat$SumOffertile, 
            nData = nrow(Dat),
            ClimateT = Dat$MeanSummerTempGrid.sc,
            WeatherT = Dat$MeanSummerTempAnnomalie.sc,
            ClimateP = Dat$AnnPrecGrid.sc,
            WeatherP = Dat$AnnPrecAnnomalie.sc,
            WeatherPrevT = Dat$MeanSummerTempPrevAnnomalie.sc,
            WeatherPrevP = Dat$AnnPrecPrevAnnomalie.sc,
            species = as.numeric(as.factor(Dat$species)),
            nSpecies = nlevels(as.factor(Dat$species)),
            siteID = as.numeric(as.factor(Dat$siteID)),
            nSite = nlevels(as.factor(Dat$siteID)),
            year = as.numeric(as.factor(Dat$year)),
            nYear = nlevels(as.factor(Dat$year)),
            eps.on = 1 ## Turns on the overdispersion term in the model
            
            # Predictions
            # N.pred = Dat$NumberOfOccurrence,
            # Fertile.pred = Dat$SumOffertile,
            # Temp.pred = Dat$summerTemperature_gridded,
            # siteID.pred = as.numeric(as.factor(Dat$siteID)),
            # species.pred = as.numeric(as.factor(Dat$species)),
            # nData.pred = 10
)

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = -2,
                            betaClimateT = 0.5,
                            betaWeatherT = 0.1,
                            betaClimateP = 0.1,
                            betaWeatherP = 0.5,
                            betaWeatherPrevT = 0.1,
                            betaWeatherPrevP = 0.1,

                            betaCTCP = 0.1,
                            # betaCTWP = 0.1,
                            # betaCPWT = 0.1,

                            speciesCoeff = rnorm(length(unique(Dat$species)), 0, 0.1),
                            siteCoeff = rnorm(length(unique(Dat$siteID)), 0, 0.1),
                            yearCoeff = rnorm(length(unique(Dat$year)), 0, 0.1)
)

para.names <- c("alpha", "betaClimateT", "betaWeatherT", "betaClimateP", "betaWeatherP", "betaWeatherPrevT", "betaWeatherPrevP", "betaCTCP", "siteCoeff")

# Run analysis
# try burnin 0, iter 100000, thining 20
res1 <- jagsUI::jags(data = Data,
             inits = inits.fn,
             parameters.to.save = para.names,
             model.file = tempFileLoc,
             n.thin = 5,
             n.chains = 3,
             n.iter = 1000,
             n.cores = 3)

plot(res1)
whiskerplot(res1, c("betaClimateT", "betaWeatherT", "betaClimateP", "betaWeatherP", "betaWeatherPrevT", "betaWeatherPrevP", "betaCTCP"))

summary_all <- res1$summary
summary <- summary_all[2:8,] %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "variable") %>% 
  mutate(variable = recode(variable, "betaClimateT" = "ClimateT", "betaClimateP" = "ClimateP", "betaWeatherT" = "WeatherT", "betaWeatherP" = "WeahterP", "betaWeatherPrevT" = "WeatherPrevT", "betaWeatherPrevP" = "WeatherPrevP", "betaCTCP" = "ClimateTxP"))
results <- tibble(ClimateT = res1$sims.list$betaClimateT,
              ClimateP = res1$sims.list$betaClimateP,
              WeatherT = res1$sims.list$betaWeatherT,
              WeahterP = res1$sims.list$betaWeatherP,
              WeatherPrevT = res1$sims.list$betaWeatherPrevT,
              WeatherPrevP = res1$sims.list$betaWeatherPrevP,
              ClimateTxP = res1$sims.list$betaCTCP)
write_csv(results, path = "Output/results.csv")
results <- read_csv(file = "Output/results.csv")

dd <- results %>% 
  pivot_longer(cols = c(ClimateT:ClimateTxP), names_to = "variable") 
#%>% mutate(variable = factor(variable, levels = c("ClimateT", "ClimateP", "ClimateTxP", "WeatherT", "WeatherP", "WeatherPrevT", "WeatherPrevP")))
ggplot(dd, aes(x = value, y = variable)) +
  geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
  geom_violin() +
  geom_point(data = summary, aes(x = mean, y = variable), colour = "red") +
  geom_errorbarh(data = summary, aes(x = mean, y = variable, xmin = `2.5%`, xmax = `97.5%`), height = 0, colour = "red") +
  labs(x = "Estimate", y = "") +
  theme_minimal()


res2 <- R2jags::jags(data = Data,
                      inits = inits.fn,
                      parameters.to.save = para.names,
                      model.file = tempFileLoc,
                      n.thin = 5,
                      n.chains = 3,
                      n.iter = 50000)

res2$BUGSoutput$sims.list


# jags.parallel(data=jags.data,
#               inits=NULL, 
#               parameters.to.save=jags.param,
#               n.iter=50000 ,
#               model.file=matdifmod,
#               n.thin=5, 
#               n.chains=3)

# predicted= alpha +betaClimateT*site climateT +....
# es[i] <- pow(cover[i]-mu[i], 2)
# cov.new[i] ~ dnorm(mu[i], prec3)
# res.new[i] <- pow(cov.new[i]-mu[i],2)
# 
# matdifmod <- function(){  
# # group effects  
# for (j in 1:12){nettstedet[j]~dnorm(0, prec1)}  
# for (j in 1:60){blokkere[j]~dnorm(0, prec2)}  
# #likelihood  
#   for (i in 1:N){    
#     cover[i]~dnorm(mu[i], prec3)    
#     mu[i] <- a[spp[i]]+ b[spp[i]]*mat[i] + nettstedet[site[i]]+       
#       blokkere[block[i]]        
#     res[i] <- pow(cover[i]-mu[i], 2)    
#     cov.new[i] ~ dnorm(mu[i], prec3)    
#     res.new[i] <- pow(cov.new[i]-mu[i],2)  }  
#   # priors  
#   for(j in 1:80){a[j]~dnorm(0, 1.0E-6)}  
#   for(j in 1:80){b[j]~dnorm(0, 1.0E-6)}  
#   prec1~dgamma(0.001,0.001)  
#   prec2~dgamma(0.001,0.001) 
#   prec3~dgamma(0.001,0.001) 
#   # derived params
#   rss <- sum(res[])
#   rss.new <- sum(res.new[])}



res1
names(res1)
res1$summary

#save(out3, file = "out3.Rdata")

# Model checking with DHARMa
jagsModel <- jags.model(file= tempFileLoc, data = Data,  
                       inits = inits.fn, 
                        n.chains = 3, n.adapt= 500)

# Sample simulated posterior for #survivors (alive.pred)
Samples <- coda.samples(jagsModel, 
                             variable.names = "betaTemp", 
                             n.iter = 500)

# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)


# Transform mcmc.list object to a matrix
Pred.Mat <- as.matrix(Pred.Samples)

# Cretae model checking plots
res = createDHARMa(simulatedResponse = t(Pred.Mat),
                   observedResponse = Dat$SumOffertile, 
                   integerResponse = T, 
                   fittedPredictedResponse = apply(Pred.Mat, 2, median))
plot(res)


### MAKE FIGURES

data.frame(unlist(res1$samples[[1]])) %>% 
  bind_rows(data.frame(unlist(res1$samples[[2]])),
            data.frame(unlist(res1$samples[[3]]))) %>% 
  as_tibble() %>% 
  gather(key = ID, value = value) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ ID, scales = "free")

names(res1)


sites <- Dat %>% 
  ungroup %>% 
  distinct(siteID, temperature_level, precipitation_level) %>% 
  mutate(ID = paste("beta.", 1:4, ".", sep = ""))


outputMod3 <- data.frame(unlist(out3$samples[[1]]))
save(outputMod3, file = "outputMod3.Rdata")

# Plot mean beta and credible interval for each site
data.frame(unlist(out3$samples[[1]])) %>% 
  bind_rows(data.frame(unlist(out3$samples[[2]])),
        data.frame(unlist(out3$samples[[3]]))) %>% 
  as_tibble() %>% 
  select(beta.1.:beta.12.) %>% 
  gather(key = ID, value = value) %>% 
  group_by(ID) %>% 
  summarise(mean = mean(value), low = quantile(value, probs = 0.025), high = quantile(value, probs = 0.975)) %>% 
  left_join(sites, by = "ID") %>% 
  mutate(temperature_level = plyr::mapvalues(temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(temperature_level = factor(temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  mutate(Pmm = plyr::mapvalues(precipitation_level, c(1, 2, 3, 4), c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  mutate(Pmm = factor(Pmm, levels =  c("500mm", "1200mm", "2000mm", "2700mm"))) %>% 
  ungroup() %>%
  ggplot(aes(x = Pmm, y = mean, ymin = low, ymax = high, color = temperature_level)) +
  geom_point(shape = 21, size = 2) +
  geom_errorbar(width = 0) +
  scale_color_manual(name = "Temperature level", values = c("#56B4E9", "#E69F00", "#D55E00")) +
  labs(x = "", y = "proportion flowering") +
  facet_grid( ~ temperature_level)


# plot mean variance and credible interval for each temp level
data.frame(unlist(out3$samples[[1]])) %>% 
  bind_rows(data.frame(unlist(out3$samples[[2]])),
            data.frame(unlist(out3$samples[[3]]))) %>% 
  as.tibble() %>% 
  select(beta.1.:beta.12.) %>% 
  rowid_to_column() %>% 
  gather(key = ID, value= value, -rowid) %>% 
  left_join(sites, by = "ID") %>% 
  group_by(rowid, temperature_level) %>% 
  summarise(var = var(value)) %>% 
  ungroup() %>% 
  group_by(temperature_level) %>% 
  summarise(mean = mean(var), low = quantile(var, probs = 0.025), high = quantile(var, probs = 0.975)) %>% 
  mutate(temperature_level = plyr::mapvalues(temperature_level, c(1,2,3), c("alpine", "subalpine", "boreal"))) %>% 
  mutate(temperature_level = factor(temperature_level, levels = c("alpine", "subalpine", "boreal"))) %>% 
  ggplot(aes(x = temperature_level, y = mean, ymin = low, ymax = high)) +
  geom_point() +
  geom_errorbar(width = 0) +
  labs(x = "")



# plot prob flowering (from predictions)




as.tibble((out2$mean$beta)) %>% 
  bind_cols(as.tibble((out2$q2.5$beta)), as.tibble((out2$q97.5$beta))) %>% 
  rename(mean = value, low = value1, high = value2) %>% 
  rowid_to_column() %>% 
  ggplot(aes(y = mean, x = rowid, ymin = low, ymax = high)) +
  geom_point() +
  geom_errorbar()
  
  

#### JAGS.MODEL
# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 1000)

# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, 
                        variable.names = para.names, 
                        n.iter = 5000, 
                        thin = 2)


# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)

library("BayesianTools")
correlationPlot(Samples)

effectiveSize(Samples)
summary(Samples)
autocorr.plot(Samples)
rejectionRate(Samples)



### TO DO
# check different models: 
# Space model with year: fl ~ (temp|year) + (1|species)
# with grand mean and slope: fl ~ temp + (temp|site) + (1|species)
# with different slopes for species: fl ~ (temp|site) + (temp|species)
# with site and year in the model fl ~ (temp|site) + (temp|year)

# run model for other variable: prec, temp and prec in previous year, snowmelt

# check again the data, which data should be included, what is missing.

# plot sigmoid curves for different sites/t_levels. Either for all points or from predicitons

# extract sp values and plot different func groups or alpine vs. generalists






# Model GLMME with gridded temperature as predictor
# Random effect: site with intercept and slope
# Time model with site: fl ~ (temp|site) + (temp|species)
# Zero inflation and overdispersion

tempFileLoc <- tempfile()
cat(
  "model{
  
  # BINOMIAL LIKELIHOOD
  for(i in 1:nData){
  Fertile[i] ~ dbin(p[i] * Inc[i], N[i])
  Inc[i] ~ dbern(p.Inc) # zero inflation
  logit(p[i]) <- alpha[siteID[i]] + beta[siteID[i]] * Temp[i] + speciesCoeff[species[i]] + eps[i] 
  
  # overdispersion term
  eps[i] ~ dnorm(0, tau.eps) 
  }
  
  
  # PRIORS
  for(i in 1:nSite){
  alpha[i] ~ dnorm(mu.int, tau.int) # Intercept
  beta[i] ~ dnorm(mu.slope, tau.slope) # Slope
  }
  
  mu.int ~ dnorm(0, 0.001)
  tau.int <- 1/(sigma.int * sigma.int)
  sigma.int ~ dunif(0, 10)
  
  mu.slope ~ dnorm(0, 0.001)
  tau.slope <- 1/(sigma.slope * sigma.slope)
  sigma.slope ~ dunif(0, 10)  
  
  for(j in 1:nSpecies) {
  speciesCoeff[j] ~ dnorm(0, randPrecSP)
  }
  randPrecSP ~ dgamma(0.001,0.001)
  
  # binary variable to indicate flowering
  p.Inc ~ dbeta(1,1)
  tau.eps ~ dgamma(0.001,0.001)
  
  # PREDICITON
  for(i in 1:nData.pred) {
  Fertile.pred[i] ~ dbin(p.pred[i] * Inc.pred[i], N.pred[i])
  Inc.pred[i] ~ dbern(p.Inc) # zero inflation
  logit(p.pred[i]) <- alpha[siteID.pred[i]] + beta[siteID.pred[i]] * Temp.pred[i] + speciesCoeff[species.pred[i]] + eps.pred[i] 
  
  # overdispersion term
  eps.pred[i] ~ dnorm(0, tau.eps) 
  }
  
  }
  ", file = tempFileLoc)





# Site model
tempFileLoc <- tempfile()
cat(
  "model{
  
  #LIKELIHOOD
  for (i in 1:nData) {
  Fertile[i] ~ dbin(p[i], N[i])
  logit(p[i]) <- alpha + betaTemp[yearID[i]] * Temp[i] + yearCoeff[yearID[i]] + siteCoeff[siteID[i]] + speciesCoeff[species[i]]  + eps[i]*eps.on
  
  # overdispersion term
  eps[i] ~ dnorm(0,tau.over)
  
  }
  
  alpha ~ dnorm(0, 0.001) # grand intercept
  tau.over ~ dgamma(0.001,0.001)
  
  # PRIORS
  for(i in 1:nSiteID){
  siteCoeff[i] ~ dnorm(site.int, site.tau) # Intercept for each site
  }
  
  for(i in 1:nSpeciesID){
  speciesCoeff[i] ~ dnorm(species.int, species.tau) # Intercept for each species
  }
  
  for(i in 1:nYearID){
  yearCoeff[i] ~ dnorm(year.int, year.tau) # Intercept for each year
  betaTemp[i] ~ dnorm(year.mu.slope, year.tau.slope) # Slope for each year
  }
  
  site.int ~ dnorm(0, 0.001)
  site.tau <- 1/(site.sigma.int * site.sigma.int)
  site.sigma.int ~ dunif(0, 10)
  
  species.int ~ dnorm(0, 0.001)
  species.tau <- 1/(sp.sigma.int * sp.sigma.int)
  sp.sigma.int ~ dunif(0, 10)
  
  year.int ~ dnorm(0, 0.001)
  year.tau <- 1/(year.sigma.int * year.sigma.int)
  year.sigma.int ~ dunif(0, 10)
  
  year.mu.slope ~ dnorm(0, 0.001)
  year.tau.slope <- 1/(sigma.slope * sigma.slope)
  sigma.slope ~ dunif(0, 10)


  # PREDICTION
  for (i in 1:nData.pred) {
  Fertile.pred[i] ~ dbin(p.pred[i], N.pred[i])
  logit(p.pred[i]) <- alpha + betaTemp[yearID.pred[i]] * Temp.pred[i] + yearCoeff[yearID.pred[i]] + siteCoeff[siteID.pred[i]] + speciesCoeff[species.pred[i]]  + eps.pred[i]*eps.on
  
  # overdispersion term
  eps.pred[i] ~ dnorm(0,tau.over) 
  
  }
  
  }
  ", file = tempFileLoc)

Data = list(N = Dat$NumberOfOccurrence,
            Fertile = Dat$SumOffertile, 
            Temp = Dat$MeanSummerTemp.sc,
            siteID = as.numeric(as.factor(Dat$siteID)),
            nSiteID = nlevels(as.factor(Dat$siteID)),
            species = as.numeric(as.factor(Dat$species)),
            nSpeciesID = nlevels(as.factor(Dat$species)),
            yearID = as.numeric(as.factor(Dat$year)),
            nYearID = nlevels(as.factor(Dat$year)),
            nData = nrow(Dat),
            eps.on = 0, # Turns on the overdispersion term in the model
            
            N.pred = Dat$NumberOfOccurrence, 
            Temp.pred = Dat$MeanSummerTemp.sc,
            species.pred = as.numeric(as.factor(Dat$species)),
            nData.pred = nrow(Dat)
)

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(1, -4, 0.1),
                            #betaTemp = rnorm(1,0.2,1),
                            betaTemp = rnorm(length(unique(Dat$siteID)), 0, 0.1),
                            siteCoeff = rnorm(length(unique(Dat$siteID)), 0, 0.1),
                            mu.int = rnorm(1, 0, 1),
                            mu.slope = rnorm(1, 0, 1)
)

para.names <- c("alpha", "betaTemp", "yearCoeff", "siteCoeff", "speciesCoeff")


jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        #init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 5000)

Samples <- coda.samples(jagsModel, 
                        variable.names = para.names,
                        n.iter = 5000)

plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)
summary(glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile)  ~ MeanSummerTemp + (MeanSummerTemp|siteID) + (MeanSummerTemp|species), data = Dat, family = "binomial", weights = NumberOfOccurrence))

ggplot(Dat, aes(x = summerTemperature_gridded.sc, y = SumOffertile)) +
  geom_point() +
  facet_wrap(~ siteID)

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)


res = expit(alpha + beta * T.sc)
unscale()
result <- summary(Samples)

res <- result$statistics %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  select(Variable, Mean)

sites <- Dat %>% 
  ungroup %>% 
  distinct(siteID, temperature_level, precipitation_level) %>% 
  mutate(Site = siteID,
         siteID = as.numeric(as.factor(siteID)),
         Site = factor(Site, levels = c("Lavisdalen", "Hogsete", "Vikesland", "Gudmedalen", "Rambera", "Arhelleren")))


res2 <- res %>% 
  as.tibble() %>% 
  filter(., grepl("alpha|site",Variable)) %>% 
  spread(key = Variable, value = Mean) %>% 
  gather(key = site, value = value, - alpha) %>% 
  mutate(mean = alpha + value,
         siteID = 1:n()) %>% 
  left_join(sites, by = "siteID")

ggplot(res2, aes(x = factor(temperature_level), y = mean, color = factor(temperature_level))) +
  geom_point(size = 3) +
  facet_grid(~ precipitation_level)

meta <- Dat %>% 
  ungroup() %>% 
  distinct(year, siteID, species)


yearstats <- res %>% 
  as.tibble() %>% 
  filter(., grepl("alpha|Temp|year",Variable)) %>% 
  spread(key = Variable, value = Mean) %>% 
  mutate(alpha1 = alpha + `yearCoeff[1]`,
         alpha2 = alpha + `yearCoeff[2]`, 
         alpha3 = alpha + `yearCoeff[3]`) %>% 
  select(- alpha, -`yearCoeff[1]`, -`yearCoeff[2]`, -`yearCoeff[3]`) %>% 
  rename(beta1 = `betaTemp[1]`, beta2 = `betaTemp[2]`, beta3 = `betaTemp[3]`) %>% 
  unite(alpha1, beta1, col = year1, sep = "_") %>% 
  unite(alpha2, beta2, col = year2, sep = "_") %>% 
  unite(alpha3, beta3, col = year3, sep = "_") %>% 
  gather(key = year, value = value) %>% 
  separate(col = value, into = c("intercept", "slope"), sep = "_", convert = TRUE) %>% 
  mutate(year = as.numeric(plyr::mapvalues(year, c("year1", "year2", "year3"), c(2009, 2011, 2013))))

nyears <- nlevels(as.factor(Dat$year))
N <- length(rep(seq(-20, 20, 0.05), nyears))/nyears

data_frame(Temp.scale = rep(seq(-20, 20, 0.05), nyears),
           year = c(rep(2009, N), rep(2011, N), rep(2013, N))) %>% 
  left_join(yearstats, by = "year") %>% 
  mutate(prob.fertile = expit(intercept + slope * Temp.scale)) %>% 
  ggplot(aes(y = prob.fertile, x = Temp.scale)) +
  geom_point() +
  facet_grid(~ year)