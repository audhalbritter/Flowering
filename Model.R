tempFileLoc <- tempfile()
cat(
  "model{
  
  # BINOMIAL LIKELIHOOD
  for(i in 1:nData){
  #Fertile[i] ~ dbin(p[i], N[i])
  Fertile[i] ~ dbin(p[i] * Inc[i], N[i])
  Inc[i] ~ dbern(p.Inc) # zero inflation
  logit(p[i]) <- alpha[siteID[i]] + betaTemp[siteID[i]] * Temp[i] + speciesCoeff[species[i]] + eps[i]*eps.on
  
  # overdispersion term
  eps[i] ~ dnorm(0,tau.over) 
  }
  
  
  # PRIORS
  for(i in 1:nSite){
  alpha[i] ~ dnorm(mu.int, tau.int) # Intercept
  betaTemp[i] ~ dnorm(mu.slope, tau.slope) # Slope for temp
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
  tau.over ~ dgamma(0.001,0.001)
  
  }
  ", file = tempFileLoc)




# PREDICITON
#for(i in 1:nData.pred) {
  #Fertile.pred[i] ~ dbin(p.pred[i] * Inc[i], N.pred[i])
  #Inc.pred[i] ~ dbern(p.Inc) # zero inflation
  #logit(p.pred[i]) <- alpha[siteID.pred[i]] + beta[siteID.pred[i]] * Temp.pred[i] + speciesCoeff[species.pred[i]] #+ eps.pred[i] 
  
  # overdispersion term
  #eps.pred[i] ~ dnorm(0, tau.eps) 
#}