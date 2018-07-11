### LOAD FERTILITY DATA FROM DATABASE ####

# load libraries
library("RSQLite")
#library("DBI")
library("tidyverse")


# Stuff
pn <- . %>% print(n = Inf)

# Colours
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


con <- dbConnect(SQLite(), dbname = "~/Dropbox/Bergen/seedclimComm/database/seedclim.sqlite")

# vies all subtables
DBI::dbListTables(conn = con)
# view column names of a table
dbListFields(con, "sites")


### Load fertility subTurf data from database
fertile <- tbl(con, "subTurfCommunity") %>% 
  select(turfID, subTurf, year, species, fertile, dominant) %>% # could also be interesting to import: seedlings, juvenile, adult, vegetative
  left_join(tbl(con, "turfs"), by = "turfID") %>% 
  
  # only control plots
  filter(TTtreat %in% c("TTC", "TT1")) %>%
  select(-RTtreat, -GRtreat, -destinationPlotID) %>% 
  
  left_join(tbl(con, "plots"), by = c("originPlotID" = "plotID")) %>% 
  left_join(tbl(con, "blocks"), by = c("blockID")) %>% 
  left_join(tbl(con, "sites"), by = c("siteID")) %>% 
  
  collect() %>% 
  
  # Calculate stuff
  group_by(turfID, year, species, siteID, blockID, originPlotID, TTtreat, temperature_level, precipitation_level, annualPrecipitation_gridded, summerTemperature_gridded) %>% 
  summarize(SumOffertile = sum(fertile), NumberOfOccurrence = n()) %>% # loos colums here, need to add in group_by above if I need to keep more columns
  mutate(PropFertile = SumOffertile / NumberOfOccurrence)


# Load taxon and trait data
traits <- tbl(con, "character_traits") %>% 
  left_join(tbl(con, "taxon", by = "species")) %>% 
  collect() %>% 
  spread(key = trait, value = value) %>% 
  select(-`Common-rear`, -Habitat, -`Soil type`) %>% 
  
  # Flowering time
  # FloweringStart: Var or FSo => early; otherwise late
  rename(FloweringFinish = `Flowering finish`, FloweringStart = `Flowering start`) %>% 
  mutate(FloweringFinish = plyr::mapvalues(FloweringFinish, c("H<f8>st", "MSo", "SSo", "FSo", "V<e5>r"), c("Host", "MSo", "SSo", "FSo", "Var"))) %>% 
  mutate(FloweringStart = plyr::mapvalues(FloweringStart, c("MSo", "SSo", "FSo", "V<e5>r"), c("MSo", "SSo", "FSo", "Var")))

  # Occurrence
  # Upper: everything but not HAlp, MAlp or Lalp => lowland
  # Lower: LAlp but not Nem, BNem, SBor => alpine


# Join fertility and trait data
fertile <- fertile %>% 
  left_join(traits, by = "species")
  

### Data curation  
fertile <- fertile %>% 
  filter(year != 2010) %>% # remove first year, because of fence effect
  # remove species that occur in less than 3 years
  group_by(turfID, species) %>% 
  mutate(nYears = n()) %>%
  filter(nYears > 3) %>% 
  filter(functionalGroup %in% c("graminoid", "forb")) %>% 
  # relative fertility (correct for species having different proportion of fertility)
  group_by(species) %>% 
  mutate(mean.fertile = mean(PropFertile)) %>% 
  mutate(rel.fertile = PropFertile / mean.fertile) %>% 
  mutate(rel.fertile = ifelse(rel.fertile == "NaN", 0 , rel.fertile))

# Get common species
commonSP <- fertile %>% 
  filter(NumberOfOccurrence > 15) %>% distinct(species)

fertile <- fertile %>%
  mutate(commonSP = ifelse(species %in% commonSP), 1, 0)


# Leave for now, but maybe also filter species (at site level) that never flower
  #group_by(siteID, species) %>% 
  #mutate(sum(SumOffertile)) %>% 
  #filter(`sum(SumOffertile)` == 0)



#### Load Climate data ####

### GRIDDED DATA
load("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedDailyClimateData2009-2017.RData", verbose=TRUE)

# Calculate monthly values (sum for prec, mean for temp)
monthlyClimate <- climate %>%
  select(Site, Date, Precipitation, Temperature) %>% 
  mutate(Date = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  gather(key = Logger, value = value, -Site, -Date) %>% 
  group_by(Date, Site, Logger) %>%
  summarise(n = n(), mean = mean(value), sum = sum(value)) %>% 
  mutate(Value = ifelse(Logger == "Precipitation", sum, mean)) %>% 
  select(-n, -sum, -mean)



# get annual values
summer <- monthlyClimate %>% 
  filter(Logger == "Temperature" & month(Date) %in% 6:9) %>%
  mutate(Year = year(Date)) %>% 
  group_by(Year, Site, Logger) %>%
  summarise(n = n(), Value = mean(Value)) %>% 
  mutate(Logger = "MeanSummerTemp") %>% 
  select(-n)


Climate <- monthlyClimate %>%
  mutate(Year = year(Date)) %>% 
  group_by(Year, Site, Logger) %>%
  summarise(n = n(), mean = mean(Value), sum = sum(Value)) %>% 
  mutate(Value = ifelse(Logger == "Precipitation", sum, mean)) %>% 
  select(-n, -sum, -mean) %>% 
  bind_rows(summer) %>% 
  spread(key = Logger, value = Value) %>% 
  rename(AnnPrec = Precipitation, MeanTemp = Temperature)


