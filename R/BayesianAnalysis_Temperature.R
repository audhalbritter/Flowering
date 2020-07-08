##############################
   ### Baysian Analysis ###
##############################

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
            eps.on = 0, ## Turns on the overdispersion term in the model
            
            # Predictions
            N.pred = Dat$NumberOfOccurrence,
            Fertile.pred = Dat$SumOffertile,
            nData.pred = nrow(Dat),
            ClimateT.pred = Dat$MeanSummerTempGrid.sc,
            WeatherT.pred = Dat$MeanSummerTempAnnomalie.sc,
            ClimateP.pred = Dat$AnnPrecGrid.sc,
            WeatherP.pred = Dat$AnnPrecAnnomalie.sc,
            WeatherPrevT.pred = Dat$MeanSummerTempPrevAnnomalie.sc,
            WeatherPrevP.pred = Dat$AnnPrecPrevAnnomalie.sc,
            species.pred = as.numeric(as.factor(Dat$species)),
            siteID.pred = as.numeric(as.factor(Dat$siteID)),
            year.pred = as.numeric(as.factor(Dat$year))
            
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



# Check model
plot(res1)
whiskerplot(res1, c("betaClimateT", "betaWeatherT", "betaClimateP", "betaWeatherP", "betaWeatherPrevT", "betaWeatherPrevP", "betaCTCP"))

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
