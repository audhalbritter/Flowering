#fl ~ climateT(grid) + climateP(grid) + WeatherT(Annomalie) + WeatherP(Annomalie) + (1|sp) + (1|year) + (1|site)

tempFileLoc <- tempfile()
cat(
  "model{
  
  # BINOMIAL LIKELIHOOD
  for(i in 1:nData){
  Fertile[i] ~ dbin(p[i], N[i])
  #Fertile[i] ~ dbin(p[i] * Inc[i], N[i])
  #Inc[i] ~ dbern(p.Inc) # zero inflation
  logit(p[i]) <- alpha 
  + betaClimateT * ClimateT[i] + betaWeatherT * WeatherT[i]
  + betaClimateP * ClimateP[i] + betaWeatherP * WeatherP[i]
  + betaWeatherPrevT * WeatherPrevT[i]
  + betaWeatherPrevP * WeatherPrevP[i]

  + betaCTCP * ClimateT[i] * ClimateP[i]
  + betaCWT * ClimateT[i] * WeatherT[i]
  + betaCWP * ClimateP[i] * WeatherP[i]

  + speciesCoeff[species[i]] 
  + siteCoeff[siteID[i]] 
  + yearCoeff[year[i]] 
  + eps[i]*eps.on
  
  # overdispersion term
  eps[i] ~ dnorm(0,tau.over) 
  }
  
  
  # PRIORS
  # fixed effects
  alpha ~ dnorm(0, 0.001) # Intercept
  betaClimateT ~ dnorm(0, 0.001) # Slope for climateT
  betaWeatherT ~ dnorm(0, 0.001) # Slope for weatherT
  betaClimateP ~ dnorm(0, 0.001) # Slope for climateP
  betaWeatherP ~ dnorm(0, 0.001) # Slope for weatherP
  betaWeatherPrevT ~ dnorm(0, 0.001) # Slope for weatherT Prev
  betaWeatherPrevP ~ dnorm(0, 0.001) # Slope for weatherP Prev

  # interactions
  betaCTCP ~ dnorm(0, 0.001) # slope for interaction ClimateT * ClimateP
  betaCWT ~ dnorm(0, 0.001) # slope for interaction ClimateT * WeatherT
  betaCWP ~ dnorm(0, 0.001) # slope for interaction ClimateP * WeatherP

  # random effects
  for(j in 1:nSpecies) {
  speciesCoeff[j] ~ dnorm(0, randPrecSP)
  }
  randPrecSP ~ dgamma(0.001,0.001)

   for(j in 1:nSite) {
   siteCoeff[j] ~ dnorm(0, randPrecSite)
   }
   randPrecSite ~ dgamma(0.001,0.001)

   for(j in 1:nYear) {
   yearCoeff[j] ~ dnorm(0, randPrecYear)
   }
   randPrecYear ~ dgamma(0.001,0.001)
  

  # binary variable to indicate flowering
  #p.Inc ~ dbeta(1,1)
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