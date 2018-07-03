### LOAD FERTILITY DATA FROM DATABASE ####
library("RSQLite")
#library("DBI")
library("tidyverse")
library("lubridate")
library("cowplot")

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
  group_by(turfID, year, species, siteID, blockID, originPlotID, TTtreat, temperature_level, precipitation_level) %>% 
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
  filter(year > 2009) %>% # remove first year, because of fence effect
  # remove species that occur in less than 3 years
  group_by(turfID, species) %>% 
  mutate(nYears = n()) %>%
  filter(nYears > 3) 
  # Leave for now, but maybe also filter species (at site level) that never flower
  #group_by(siteID, species) %>% 
  #mutate(sum(SumOffertile)) %>% 
  #filter(`sum(SumOffertile)` == 0)
