# Model Check

tempFileLoc <- tempfile()
cat(
  "model{
  
  # BINOMIAL LIKELIHOOD
  for(i in 1:nData){
  Fertile[i] ~ dbin(p[i], N[i])

  logit(p[i]) <- alpha + 
  betaClimateT * ClimateT[i] +
  speciesCoeff[species[i]] +
  eps[i] * eps.on

  # overdispersion term
  eps[i] ~ dnorm(0, tau.over) 
  }
  
  
  # PRIORS
  # fixed effects
  alpha ~ dnorm(0, 0.001) # Intercept
  betaClimateT ~ dnorm(0, 0.001) # Slope for climateT

  # random effects
  for(j in 1:nSpecies) {
  speciesCoeff[j] ~ dnorm(0, randPrecSP)
  }
  randPrecSP ~ dgamma(0.001,0.001)
  
  # binary variable to indicate flowering
  tau.over ~ dgamma(0.001,0.001)
  
  
  # PREDICITONS
  for(i in 1:nData.pred){
  Fertile.pred[i] ~ dbin(p.pred[i], N.pred[i])

  logit(p.pred[i]) <- alpha + 
  betaClimateT * ClimateT.pred[i] + 
  speciesCoeff[species.pred[i]] + 
  eps.pred[i] * eps.on

  # overdispersion term
  eps.pred[i] ~ dnorm(0, tau.over) 
  }
  
  }
  ", file = tempFileLoc)


Dat <- fertile %>% 
  filter(!year %in% c(2009))

Data = list(N = Dat$NumberOfOccurrence,
            Fertile = Dat$SumOffertile, 
            nData = nrow(Dat),
            ClimateT = Dat$MeanSummerTempGrid.sc,
            species = as.numeric(as.factor(Dat$species)),
            nSpecies = nlevels(as.factor(Dat$species)),
            eps.on = 1, ## Turns on the overdispersion term in the model
            
            # Predictions
            N.pred = Dat$NumberOfOccurrence,
            Fertile.pred = Dat$SumOffertile,
            nData.pred = nrow(Dat),
            ClimateT.pred = Dat$MeanSummerTempGrid.sc,
            species.pred = as.numeric(as.factor(Dat$species))

)

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = -2,
                            betaClimateT = 0.5,
                            speciesCoeff = rnorm(length(unique(Dat$species)), 0, 0.1)
)

para.names <- c("alpha", "betaClimateT")

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



# Check model
plot(res1)
whiskerplot(res1, c("betaClimateT"))

library("rjags")
jagsModel <- jags.model(file = tempFileLoc,
                        data = Data,  
                        inits = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 1000)

# Model checking with DHARMa
# Sample simulated posterior for #survivors (alive.pred)
Pred.Samples <- coda.samples(jagsModel, 
                             variable.names = "Fertile.pred", 
                             n.iter = 1000)

# Transform mcmc.list object to a matrix
Pred.Mat <- as.matrix(Pred.Samples)

# Cretae model checking plots
res = createDHARMa(simulatedResponse = t(Pred.Mat),
                   observedResponse = Dat$SumOffertile, 
                   integerResponse = T, 
                   fittedPredictedResponse = apply(Pred.Mat, 2, median))
plot(res)
