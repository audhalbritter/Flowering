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
library("DHARMa")

library("knitr")
library("bookdown")
library("citr")

# library("R2jags")

# library("lme4")
# library("vegan")
# library("patchwork")

# tricks
pn <- . %>% print(n = Inf)

# Colours
# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### DRAKE CONFIGURATIONS
pkgconfig::set_config("drake::strings_in_dots" = "literals")

theme_set(theme_bw(base_size = 15))


### SOURCE FUNCTIONS
source("R/Load Data from database.R")
source("R/Methods.R")
#source("R/SaunaModel.R")
#source("R/BayesianAnalysis_Temperature.R")


### DRAKE PLANS
ImportDrakePlan <- drake::drake_plan(
  # DownloadData from OSF!!!
  
  # Load
  fertile_raw = ImportFertility(),
  meta = MakeMeta(fertile_raw),
  traits = ImportTraits(),
  sites = ImportSite(),
  Climate = ImportClimate(meta),
  
  # Curate
  fertile = CombineandCurate(fertile_raw, Climate, traits)
)


MethodDrakePlan <- drake::drake_plan(
  Table1 = MakeTable1(sites),
  
  #Map = MakeMap(sites),
  ClimatePlot = MakeClimatePlot(Climate, sites)
)



# AnalysisDrakePlan <- drake::drake_plan(
#   fertile_raw = ImportFertilityData(),


# knit manuscript
ManuscriptDrakePlan <- drake::drake_plan(
  manuscript = {
    rmarkdown::render(
      input = knitr_in("Writing/Flowering_manuscript.Rmd"),
      knit_root_dir = "../",
      clean = FALSE)
  }
)


### COMBINING THE DRAKE PLANS 
MasterDrakePlan <-  bind_rows(ImportDrakePlan,
                              MethodDrakePlan,
                              ManuscriptDrakePlan) 

#DataAnalysisDrakePlan

#configure and make drake plan
config <- drake::drake_config(MasterDrakePlan)

config
