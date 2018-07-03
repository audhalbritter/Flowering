Fertility data from SeedClim project

Data:
Proportion of fertile plant species per turf (PropFertile = SumOffertile / NumberOfOccurrence in subplot)
Years: 2009 2010 2011 2012 2013 2015 2016 2017
Species: 200 graminoids and forbs


Data handling:
Only look at control plots (no transplants)
Remove first year data, because of fence effect
Only species that occure in more than 3 years (maybe adjust to 4 or 5 years)
Maybe remove species that never flower


Questions and Hypotheses:
1) Does proportion of fertility differ across years?
Yes, we expect difference in the propotion of fertility for single species and the whole plant community.

2) Does variation in the proportion of fertility differ across the climate grid?
Yes, we expect more variation in warmer (boreal) and wetter sites and less variation in alpine and dryer sites.

3) Is the temporal variation in proportion of fertility related to mean summer temp, mean temp, annual precipitation, previous year summer temperature, previous year annual precipitation, snowmelt time?

4) Does proportion of fertility (and variation) differ between plant functional groups (graminoids vs. forbs)?
Yes, we expect graminoids to have a higher proportion of flowering. 


Analyses:
1) GLMM (binomial) with proportion fertile (fertile / sum of occurrence) as response, year as fixed effect (factor) and species and blockID as random effects.

2) GLMM (binomial) with proportion fertile (fertile / sum of occurrence) as response, (year?), temperature level * precipitation level as fixed effect (factor) and species and blockID as random effects.

3) GLMM (binomial) with proportion fertile (fertile / sum of occurrence) as response, climate variables as fixed effect (factor) and species and blockID as random effects.
Use some sort of model selection to find which variables are important.

3) GLMM (binomial) with proportion fertile (fertile / sum of occurrence) as response, functionalGroup as fixed effect (factor) and species and blockID as random effects.

