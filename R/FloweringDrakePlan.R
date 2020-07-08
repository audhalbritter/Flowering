##############################
 ### Flowering Drake Plan ###
##############################

### LOAD LIBRARIES
#devtools::install_github("Between-the-Fjords/dataDownloader")
#library("dataDownloader")
library("RSQLite")
library("tidyverse")
library("lubridate")
library("jagsUI")
library("rjags")

library("knitr")
library("bookdown")
library("citr")

library("R2jags")
library("DHARMa")
library("lme4")
library("vegan")
library("patchwork")

# tricks
pn <- . %>% print(n = Inf)

# Colours
# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### DRAKE CONFIGURATIONS
pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_bw(base_size = 15))

### LOAD DATA FUNCTION
source("R/Load Data from database.R")

### DATA ANALYSIS
source("R/SaunaModel.R")


### IMPORT DRAKE PLANS
source("R/ImportPrettyDataPlan.R")
source("R/DataProcessingDrakePlan.R")
source("R/DataAnalyisDrakePlan.R")
source("R/MakePrettyFiguresPlan.R")





### COMBINING THE DRAKE PLANS 
MasterDrakePlan <-  bind_rows(ImportDrakePlan, 
                              CountryListAndTraitMeanDrakePlan,
                              DataProcessingDrakePlan,
                              DataAnalysisDrakePlan,
                              MakePrettyFiguresPlan) 

#DataAnalysisDrakePlan

#configure and make drake plan
config <- drake_config(MasterDrakePlan)

config