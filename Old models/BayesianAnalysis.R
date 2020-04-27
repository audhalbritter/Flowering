###########################
 #### BINOMIAL MODEL ####
###########################

#### LOAD LIBRARIES ####
library("tidyverse")
library("rjags")
library("jagsUI")
library("DHARMa")
library("lme4")

# logit-link function and inverse
logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

#### LOAD DATA ####
load(file = "FloweringData.Rdata")

Dat <- fertile %>% 
  filter(year %in% c(2013),
         siteID %in% c("Lavisdalen", "Hogsete", "Vikesland", "Gudmedalen", "Rambera", "Arhelleren")) %>% 
  #mutate(MeanSummerTemp.sc = scale(MeanSummerTemp)) %>% 
  select(siteID, blockID, species, NumberOfOccurrence, SumOffertile, MeanSummerTemp, year, temperature_level, precipitation_level)

Temp.sc <- scale(Dat$MeanSummerTemp) %>% as.tibble()
Dat <- Dat %>% bind_cols(Temp.sc) %>% 
  rename(MeanSummerTemp.sc = V1)
save(Dat, file = "FloweringData.Rdata")



#### Simple binomial model ####
tempFileLoc <- tempfile()
cat(
  "model{
  
  #Likelihood
for (i in 1:nData) {
  # binomial
  Fertile[i] ~ dbin(p, N[i])
  }
  
  #priors
  p ~ dbeta(a,b)

  }
  ", file = tempFileLoc)

Data = list(N = Dat$NumberOfOccurrence, 
            Fertile = Dat$SumOffertile, 
            a = 2,
            b = 2,
            nData = nrow(Dat))

inits.fn <- function() list(p = rnorm(1, 0.1, 0.01))

jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 1000)

Samples <- coda.samples(jagsModel, 
                        variable.names = c("p"), 
                        n.iter = 500)

# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)
summary(glm(PropFertile  ~ 1, data = Dat, family = "binomial", weights = NumberOfOccurrence))

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)




#*****************************************************************************
#### BINOMIAL GLMER ####
## Summer temperature as fixed effect, species as random effects
## Random intercept and slope for species

tempFileLoc <- tempfile()
cat(
  "model{
  
  #LIKELIHOOD
  for (i in 1:nData) {
  
  Fertile[i] ~ dbin(p[i], N[i])
  logit(p[i]) <- alpha + betaTemp[species[i]] * Temp[i] + speciesCoeff[species[i]]
  
  }
  
  alpha ~ dnorm(0, 0.001) # grand intercept

  # PRIORS
  for(i in 1:nSpecies){
  speciesCoeff[i] ~ dnorm(mu.int, tau.int) # Intercept for each species
  betaTemp[i] ~ dnorm(mu.slope, tau.slope) # Slope for each species
  }
  
  mu.int ~ dnorm(0, 0.001)
  tau.int <- 1/(sigma.int * sigma.int)
  sigma.int ~ dunif(0, 10)
  
  mu.slope ~ dnorm(0, 0.001)
  tau.slope <- 1/(sigma.slope * sigma.slope)
  sigma.slope ~ dunif(0, 10) 
}
  
  ", file = tempFileLoc)

  
Data = list(N = Dat$NumberOfOccurrence,
            Fertile = Dat$SumOffertile, 
            Temp = Dat$MeanSummerTemp,
            species = as.numeric(as.factor(Dat$species)),
            nSpecies = nlevels(as.factor(Dat$species)),
            nData = nrow(Dat))

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(1,1,1),
                            betaTemp = rnorm(length(unique(Dat$species)), 0, 0.1),
                            speciesCoeff = rnorm(length(unique(Dat$siteID)), 0, 0.1),
                            mu.int = rnorm(1, 0, 1),
                            mu.slope = rnorm(1, 0, 1)
)

para.names <- c("alpha", "betaTemp", "sigma.int")


jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        #init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 10000)

Samples <- coda.samples(jagsModel, 
                        variable.names = para.names,
                        n.iter = 10000)

# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)
summary(glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile)  ~ MeanSummerTemp + (MeanSummerTemp|siteID) + (1|species), data = Dat, family = "binomial", weights = NumberOfOccurrence))

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)



#*****************************************************************************
#### CORRECTION FOR OVERDISPERSION AND ZERO INFLATION ####
tempFileLoc <- tempfile()
cat(
  "model{
  
  #LIKELIHOOD
  for (i in 1:nData) {
  Fertile[i] ~ dbin(p[i] * Inc[i], N[i])
  Inc[i] ~ dbern(p.Inc) # zero inflation
  logit(p[i]) <- alpha + betaTemp[species[i]] * Temp[i] + speciesCoeff[species[i]] + eps[i]*eps.on
  
  # overdispersion term
  eps[i] ~ dnorm(0,tau.over) 
  
  }
  
  alpha ~ dnorm(0, 0.001) # grand intercept
  tau.over ~ dgamma(0.001,0.001)
  p.Inc ~ dbeta(1,1)
  
  # PRIORS
  for(i in 1:nSpecies){
  speciesCoeff[i] ~ dnorm(mu.int, tau.int) # Intercept for each species
  betaTemp[i] ~ dnorm(mu.slope, tau.slope) # Slope for each species
  }
  
  mu.int ~ dnorm(0, 0.001)
  tau.int <- 1/(sigma.int * sigma.int)
  sigma.int ~ dunif(0, 10)
  
  mu.slope ~ dnorm(0, 0.001)
  tau.slope <- 1/(sigma.slope * sigma.slope)
  sigma.slope ~ dunif(0, 10) 

  # PREDICTION
  for (i in 1:nData.pred) {
  
  Fertile.pred[i] ~ dbin(p.pred[i]* Inc.pred[i], N.pred[i])
  Inc.pred[i] ~ dbern(p.Inc) # zero inflation
  logit(p.pred[i]) <- alpha + betaTemp[species.pred[i]] * Temp.pred[i] + speciesCoeff[species.pred[i]] + eps.pred[i]*eps.on
  
  # overdispersion term
  eps.pred[i] ~ dnorm(0,tau.over) 
  
  }

}
  ", file = tempFileLoc)


Data = list(N = Dat$NumberOfOccurrence,
            Fertile = Dat$SumOffertile, 
            Temp = Dat$MeanSummerTemp,
            species = as.numeric(as.factor(Dat$species)),
            nSpecies = nlevels(as.factor(Dat$species)),
            nData = nrow(Dat),
            eps.on = 1, # Turns on the overdispersion term in the model
            
            N.pred = Dat$NumberOfOccurrence, 
            Temp.pred = Dat$MeanSummerTemp,
            species.pred = as.numeric(as.factor(Dat$species)),
            nData.pred = nrow(Dat)
            )

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(1,1,1),
                            betaTemp = rnorm(length(unique(Dat$species)), 0, 0.1),
                            speciesCoeff = rnorm(length(unique(Dat$species)), 0, 0.1),
                            mu.int = rnorm(1, 0, 1),
                            mu.slope = rnorm(1, 0, 1),
                            tau.over = 1,
                            p.Inc = rbeta(1,1,1),
                            Inc = rep(1, nrow(Dat))
)

para.names <- c("alpha", "beta", "sigma.int")


jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 5000)

Samples <- coda.samples(jagsModel, 
                        variable.names = para.names,
                        n.iter = 5000)

# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# Statistical summaries of the posterior sample for p
summary(Samples)
summary(glmer(cbind(SumOffertile, NumberOfOccurrence - SumOffertile)  ~ MeanSummerTemp + (MeanSummerTemp|siteID) + (1|species), data = Dat, family = "binomial", weights = NumberOfOccurrence))

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)



# Sample simulated posterior for #survivors (alive.pred)
Pred.Samples <- coda.samples(jagsModel, 
                             variable.names = "Fertile.pred", 
                             n.iter = 5000)

# Transform mcmc.list object to a matrix
Pred.Mat <- as.matrix(Pred.Samples)

# Cretae model checking plots
res = createDHARMa(simulatedResponse = t(Pred.Mat),
                   observedResponse = Dat$SumOffertile, 
                   integerResponse = T, 
                   fittedPredictedResponse = apply(Pred.Mat, 2, median))
plot(res)



# Run analysis using jagsUI
out <- jags(data = Data,
            inits = inits.fn,
            parameters.to.save = para.names,
            model.file = tempFileLoc,
            n.chains = 1,
            n.adapt = 100,
            n.iter = 1000,
            n.burnin = 500,
            n.thin = 2,
            n.cores = 3)



plot(out)
print(out, dig = 3)



### Questions
# What to do about site? Include T and P?
# grand slope, ok?
# plot sigmoid curves for different sites/t_levels from predicitons

tempFileLoc <- tempfile()
cat(
  "model{
  
  #LIKELIHOOD
  for (i in 1:nData) {
  Fertile[i] ~ dbin(p[i] * Inc[i], N[i])
  Inc[i] ~ dbern(p.Inc) # zero inflation
  logit(p[i]) <- alpha + beta * Temp[i] + betaTemp[species[i]] * Temp[i] + speciesCoeff[species[i]] + eps[i]*eps.on
  
  # overdispersion term
  eps[i] ~ dnorm(0,tau.over) 
  
  }
  
  alpha ~ dnorm(0, 0.001) # grand intercept
  beta ~ dnorm(0, 0.001) # grand slope
  tau.over ~ dgamma(0.001,0.001)
  p.Inc ~ dbeta(1,1)
  
  # PRIORS
  for(i in 1:nSpecies){
  speciesCoeff[i] ~ dnorm(mu.int, tau.int) # Intercept for each species
  betaTemp[i] ~ dnorm(mu.slope, tau.slope) # Slope for each species
  }
  
  mu.int ~ dnorm(0, 0.001)
  tau.int <- 1/(sigma.int * sigma.int)
  sigma.int ~ dunif(0, 10)
  
  mu.slope ~ dnorm(0, 0.001)
  tau.slope <- 1/(sigma.slope * sigma.slope)
  sigma.slope ~ dunif(0, 10) 
  
  # PREDICTION
  for (i in 1:nData.pred) {
  
  Fertile.pred[i] ~ dbin(p.pred[i]* Inc.pred[i], N.pred[i])
  Inc.pred[i] ~ dbern(p.Inc) # zero inflation
  logit(p.pred[i]) <- alpha + betaTemp[species.pred[i]] * Temp.pred[i] + speciesCoeff[species.pred[i]] + eps.pred[i]*eps.on
  
  # overdispersion term
  eps.pred[i] ~ dnorm(0,tau.over) 
  
  }
  
  }
  ", file = tempFileLoc)



