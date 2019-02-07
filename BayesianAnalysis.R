library("rjags")
library("jagsUI")
library("DHARMa")

# logit-link function and inverse
logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

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

Data = list(N = fertile$NumberOfOccurrence, 
            Fertile = fertile$SumOffertile, 
            a = 2,
            b = 2,
            nData = nrow(fertile))

inits.fn <- function() list(p = rnorm(1, 0.1, 0.01))

jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 1000)


# Add Year as covariate
cat(
  "model{
  
  #Likelihood
  for (i in 1:nData) {
  # binomial
  Fertile[i] ~ dbin(p[i], N[i])
  # linear predictor 
  logit(p[i]) <- alpha + beta.Year*Year[i]
  }
  
  #priors
  alpha ~ dnorm(0,0.001)
  beta.Year ~ dnorm(0,0.001)
  
  }
  ", file = tempFileLoc)

Data = list(N = fertile$NumberOfOccurrence, 
            Fertile = fertile$SumOffertile, 
            Year = as.numeric(fertile$year),
            nData = nrow(fertile))

inits.fn <- function() list(alpha = rnorm(1, 0.1, 0.05), 
                            beta.Year = rnorm(1, 0.015, 0.01))
inits.fn <- function() list(alpha = -1, 
                            beta.Year = 0)


jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 1000)



# Add eps to correct for overdispersion
tempFileLoc <- tempfile()
cat(
  "model{

#Likelihood
for (i in 1:nData) {
# binomial
  Fertile[i] ~ dbin(p[i], N[i])
  # linear predictor und logit link function
  logit(p[i]) <- alpha + beta.Year*Year[i] + eps[i]

  # overdispersion
  eps[i] ~ dnorm(0,tau.eps) 
 
  }
  
  #priors
  alpha ~ dnorm(0,0.001)
  beta.Year ~ dnorm(0,0.001)
  tau.eps ~ dgamma(0.001,0.001)

  }
", file = tempFileLoc)



# Prediction
for (i in 1:nData.pred) {
  # binomial
  Fertile.pred[i] ~ dbin(p.pred[i], N.pred[i])
  # logit link function
  logit(p.pred[i]) <- mu.pred[i] + eps.pred[i]
  # linear predictor 
  mu.pred[i] <- alpha + beta.Year*Year.pred[i]
  
  # Overdispersion error term
  eps.pred[i] ~ dnorm(0,tau.eps)
}



# 2) Set up a list that contains all the necessary data.
# (here including parameters for the prior distribution)
# Parameters for prior
Data = list(N = fertile$NumberOfOccurrence,
            Fertile = fertile$SumOffertile, 
            Year = fertile$year,
            nData = nrow(fertile),
            
            N.pred = fertile$NumberOfOccurrence,
            Fertile.pred = fertile$SumOffertile, 
            Year.pred = fertile$year,
            nData.pred = nrow(fertile))

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(1), 
                            beta.Year = rnorm(1)#, 
                            #tau.eps = runif(1,1,10)
                            )

inits.fn <- function() list(alpha = 0.1, 
                            beta.Year = 0.01#, 
                            #tau.eps = runif(1,1,10)
)


# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= tempFileLoc, 
                        data=Data, 
                        init = inits.fn, 
                        n.chains = 3, 
                        n.adapt= 5000)

# Specify parameters for which posterior samples are saved
#para.names <- c('p')
para.names <- c('alpha','beta.Year', 'tau.eps')
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, 
                        n.iter = 5000)


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

# For further analyses it is often useful to
# transform mcmc.list object to a matrix
Pars.Mat <- as.matrix(Samples)
hist(Pars.Mat[,'beta.Year'], freq = FALSE, col = 'blue', breaks = 30)
# Typical statistics we want to calculate for the
# posterior sample are the median and the 
# limits of the 95% confidence interval
p.med <- median(Pars.Mat[,'beta.Year'])
abline(v = p.med, lty = 2, lwd = 2)
CI <- quantile(Pars.Mat[,'beta.Year'], prob = c(0.025,0.975))
abline(v = CI, lty = 2)



### Check predictions
Pred.Samples <- coda.samples(jagsModel, variable.names = "Fertile.pred", 
                        n.iter = 5000)

# Transform mcmc.list object to a matrix
Pred.Mat <- as.matrix(Pred.Samples)

# Make a histogram for the prediciton for first population (alive)
hist(Pred.Mat[,1])
# add a line from the data
abline(v = fertile$SumOffertile[1], col = "red")
# Standardize (quantalie) residuals
# Which prop of pred are larger than observed value
mean(Pred.Mat[,1] > fertile$SumOffertile[1])


# Cretae model checking plots
# use the data predicted with the model to compare to the actual data
res = createDHARMa(simulatedResponse = t(Pred.Mat),
                   observedResponse = fertile$SumOffertile[1:100], 
                   integerResponse = T, 
                   fittedPredictedResponse = apply(Pred.Mat, 2, median))
plot(res)




# GLMER with Year as fixed effect, species and blockID as random effects
# Overdispersion and zero inflation
# BlockID nested in site -> site x block

# Model
tempFileLoc <- tempfile()
cat(
  "model{
  
  #LIKELIHOOD
  for (i in 1:nData) {
  
  #binomial model 
  # with correction for overdispersion and zero inflation
  #Fertile[i] ~ dbin(p[i], N[i])
  Fertile[i] ~ dbin(p[i] * Inc[i], N[i])
  Inc[i] ~ dbern(p.Inc) # zero inflation

  # linear predictor with year as fixed and species and block as random effects
  # logit link function
  #logit(p[i]) <- beta.Year[Year[i]]
  logit(p[i]) <- beta.Year[Year[i]] + speciesCoeff[species[i]] + siteCoeff[siteID[i]] + blockCoeff[blockID[i]] + eps[i] 

  # Fix overdispersion
  eps[i] ~ dnorm(0, tau.eps) 
  }
  
  # PRIORS
  # Fixed effects
  for(k in 1:n.year){
  beta.Year[k] ~ dnorm(0,0.001)
  }
 
  # Random effects
  for(j in 1:nSpecies) {
  speciesCoeff[j] ~ dnorm(0, randPrecSP)
  }
  randPrecSP ~ dgamma(0.001,0.001)
  
  for(k in 1:nBlock) {
  blockCoeff[k] ~ dnorm(0, randPrecBlock)
  }
  randPrecBlock ~ dgamma(0.001,0.001)
  
  for(l in 1:nSite) {
  siteCoeff[l] ~ dnorm(0, randPrecSite)
  }
  randPrecSite ~ dgamma(0.001,0.001)

  # binary variable to indicate flowering
  p.Inc ~ dbeta(1,1)
  tau.eps ~ dgamma(0.001,0.001)

  }

  ", file = tempFileLoc)

  


Dat <- fertile %>% 
  filter(year %in% c(2009, 2011, 2012))

Data = list(N = Dat$NumberOfOccurrence,
            Fertile = Dat$SumOffertile, 
            Year = as.numeric(factor(Dat$year)),
            species = as.numeric(as.factor(Dat$species)),
            nSpecies = nlevels(as.factor(Dat$species)),
            blockID = as.numeric(as.factor(Dat$blockID)),
            nBlock = nlevels(as.factor(Dat$blockID)),
            siteID = as.numeric(as.factor(Dat$siteID)),
            nSite = nlevels(as.factor(Dat$siteID)),
            nData = nrow(Dat),
            n.year = length(unique(Dat$year)),
            
            N.pred = Dat$NumberOfOccurrence,
            Fertile.pred = Dat$SumOffertile,
            Year.pred = as.numeric(as.factor(Dat$year)),
            species.pred = as.numeric(as.factor(Dat$species)),
            blockID.pred = as.numeric(as.factor(Dat$blockID)),
            nData.pred = nrow(Dat)
            )

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(beta.Year = rnorm(3, 0, 0.1),
                            p.Inc = rbeta(1, 1, 1),
                            Inc = rep(1, nrow(Dat)),
                            tau.eps = 1,
                            speciesCoeff = rnorm(length(unique(Dat$species)), 0, 0.1),
                            siteCoeff = rnorm(length(unique(Dat$siteID)), 0, 0.1),
                            blockCoeff = rnorm(length(unique(Dat$blockID)), 0, 0.1)
)



para.names <- c('beta.Year')
#para.names <- c('beta.Year', 'tau.eps', 'speciesCoeff')

# Run analysis
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

str(out)
names(out)

out$samples[[1]]



gelman.diag(Samples)
gelman.plot(Samples)



Pred.Samples <- coda.samples(jagsModel1, 
                             variable.names = "Fertile.pred", 
                             n.iter = 5000)

Pred.Mat <- as.matrix(Pred.Samples)

res = createDHARMa(simulatedResponse = t(Pred.Mat),
                   observedResponse = Dat$SumOffertile, 
                   integerResponse = T, 
                   fittedPredictedResponse = apply(Pred.Mat, 2, median))
plot(res)







# PREDICTIONS
for (i in 1:nData.pred) {
  # binomial
  Fertile.pred[i] ~ dbin(p.pred[i], N.pred[i])
  # linear predictor and logit link function
  logit(p.pred[i]) <- interceptCoeff + beta.Year[Year.pred[i]] + speciesCoeff[species.pred[i]] + blockCoeff[blockID.pred[i]] + eps.pred[i]
  
  # Overdispersion error term
  eps.pred[i] ~ dnorm(0, tau.eps)  }
