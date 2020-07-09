##############################
 ### Import Function ###
##############################

# Needs to be uploaded first!!!
DownloadData <- function(){
  ### DOWNLOAD DATABASE FROM OSF ###
  # add function here
  
  ### DOWNLOAD CLIMATE DATA ###
  # get_file(node = "npfa9", 
  #          file = "GriddedDailyClimateData2009-2019.csv", 
  #          path = "data_cleaned")
}


### MAKE CONNCECTIO TO DATABASE ####
# DatabaseConnection <- function(){
# 
#   con <- dbConnect(SQLite(), dbname = "~/Dropbox/Bergen/seedclimComm/database/seedclim.sqlite")
#   # vies all subtables
#   #DBI::dbListTables(conn = con)
#   # view column names of a table
#   #dbListFields(con, "sites")
# 
#   return(con)
# }


### LOAD DATA FROM DATABASE ###
ImportFertility <- function(){
  con <- dbConnect(SQLite(), dbname = "~/Dropbox/Bergen/seedclimComm/database/seedclim.sqlite")
  
  fertile_raw <- tbl(con, "subTurfCommunity") %>% 
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
    mutate(PropFertile = SumOffertile / NumberOfOccurrence) %>% 
    mutate(Experiment = ifelse(is.na(TTtreat), "RTC", "SeedClim"))
  
  return(fertile_raw)
}

MakeMeta <- function(fertile_raw){
  meta <- fertile_raw %>% 
    ungroup() %>% 
    distinct(siteID, summerTemperature_gridded, annualPrecipitation_gridded)
  
  return(meta)
}

ImportTraits <- function(){
  con <- dbConnect(SQLite(), dbname = "~/Dropbox/Bergen/seedclimComm/database/seedclim.sqlite")
  
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

}


ImportSite <- function(){
  con <- dbConnect(SQLite(), dbname = "~/Dropbox/Bergen/seedclimComm/database/seedclim.sqlite")
  
  # Load site details
  sites.raw <- tbl(con, "sites")
  
  sites <- sites.raw %>% 
    select(siteID, latitude, longitude, `altitude(DEM)`, annualPrecipitation_gridded, temperature_level, summerTemperature_gridded, precipitation_level) %>% 
    rename("elevation_masl" = `altitude(DEM)`) %>% 
    mutate(temperature_level2 = case_when(temperature_level == "1" ~ "alpine",
                                         temperature_level == "2" ~ "subalpine",
                                         temperature_level == "3" ~ "boreal"),
           precipitation_level2 = as.character(case_when(precipitation_level == "1" ~ "600",
                                           precipitation_level == "2" ~ "1200",
                                           precipitation_level == "3" ~ "2000",
                                           precipitation_level == "4" ~ "2700"))) %>% 
    mutate(precipitation_level2 = factor(precipitation_level2, levels = c("600", "1200", "2000", "2700")))
  
  return(sites)
}



### LOAD WEATHER DATA FROM MODEL
ImportClimate <- function(meta){
  climate <- read_csv(file = "data_cleaned/GriddedDailyClimateData2009-2019.csv")
  
  # Calculate monthly values (sum for prec, mean for temp)
  monthlyClimate <- climate %>%
    select(Site, Date, Precipitation, Temperature) %>% 
    mutate(Date = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
    pivot_longer(cols = c(Temperature, Precipitation), names_to = "Logger", values_to = "value") %>% 
    group_by(Date, Site, Logger) %>%
    summarise(n = n(), mean = mean(value), sum = sum(value)) %>% 
    mutate(Value = ifelse(Logger == "Precipitation", sum, mean)) %>% 
    select(-n, -sum, -mean)
  
  # get annual values
  summer <- monthlyClimate %>% 
    filter(Logger == "Temperature" & month(Date) %in% 6:9) %>%
    mutate(Year2 = year(Date)) %>% 
    group_by(Year2, Site, Logger) %>%
    summarise(n = n(), Value = mean(Value)) %>% 
    mutate(Logger = "MeanSummerTemp") %>% 
    select(-n)
  
  Climate <- monthlyClimate %>%
    mutate(Year = year(Date)) %>% 
    mutate(Year2 = if_else(month(Date) > 7, Year + 1, Year)) %>% 
    group_by(Year2, Site, Logger) %>%
    summarise(n = n(), Value = sum(Value)) %>% 
    filter(Logger == "Precipitation") %>% 
    select(-n) %>% 
    bind_rows(summer) %>% 
    spread(key = Logger, value = Value) %>% 
    rename(AnnPrec = Precipitation, Year = Year2) %>% 
    ungroup() %>% 
    mutate(Site = recode(Site, Ulv = "Ulvhaugen", Hog = "Hogsete", Vik = "Vikesland", Gud = "Gudmedalen", Ram = "Rambera", Arh = "Arhelleren", Skj = "Skjellingahaugen", Ves = "Veskre", Alr = "Alrust", Ovs = "Ovstedal", Fau = "Fauske", Lav = "Lavisdalen"))
  
  # Previous year climate
  ClimatePrev <- Climate %>% 
    mutate(Year = Year + 1) %>% 
    rename(MeanSummerTempPrev = MeanSummerTemp, AnnPrecPrev = AnnPrec)
  
  # Annomalies, centre and scale
  Climate <- Climate %>% 
    left_join(ClimatePrev, by = c("Year", "Site")) %>% 
    left_join(meta, by = c("Site" = "siteID")) %>% 
    
    # Calculate annomalies
    mutate(AnnPrecAnnomalie = AnnPrec - annualPrecipitation_gridded,
           AnnPrecPrevAnnomalie = AnnPrecPrev - annualPrecipitation_gridded,
           MeanSummerTempAnnomalie = MeanSummerTemp - summerTemperature_gridded,
           MeanSummerTempPrevAnnomalie = MeanSummerTempPrev - summerTemperature_gridded) %>% 
    # centre and scale data
    mutate(AnnPrecGrid.sc = as.vector(scale(annualPrecipitation_gridded)),
           MeanSummerTempGrid.sc = as.vector(scale(summerTemperature_gridded)),
           AnnPrec.sc = as.vector(scale(AnnPrec)),
           MeanSummerTemp.sc = as.vector(scale(MeanSummerTemp)),
           AnnPrecPrev.sc = as.vector(scale(AnnPrecPrev)),
           MeanSummerTempPrev.sc = as.vector(scale(MeanSummerTempPrev))) %>% 
    mutate(AnnPrecAnnomalie.sc = as.vector(scale(AnnPrecAnnomalie)),
           MeanSummerTempAnnomalie.sc = as.vector(scale(MeanSummerTempAnnomalie)),
           AnnPrecPrevAnnomalie.sc = as.vector(scale(AnnPrecPrevAnnomalie)),
           MeanSummerTempPrevAnnomalie.sc = as.vector(scale(MeanSummerTempPrevAnnomalie))) %>% 
    mutate(Temp_level = case_when(Site %in% c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen") ~ "alpine",
                                Site %in% c("Alrust", "Hogsete", "Rambera", "Veskre") ~ "subalpine",
                                Site %in% c("Fauske", "Vikesland", "Arhelleren", "Ovstedal") ~ "boreal"),
           Prec_level = case_when(Site %in% c("Ulvhaugen", "Alrust", "Fauske") ~ "600mm",
                                Site %in% c("Lavisdalen", "Hogsete", "Vikesland") ~ "1200mm",
                                Site %in% c("Gudmedalen", "Arhelleren", "Rambera") ~ "2000mm",
                                Site %in% c("Skjellingahaugen", "Veskre", "Ovstedal") ~ "2700mm")) %>% 
    mutate(Prec_level = factor(Prec_level, levels = c("600mm", "1200mm", "2000mm", "2700mm")))
  
  return(Climate)
}


CombineandCurate <- function(fertile_raw, Climate, traits){
  # join climate data
  fertile_raw <- fertile_raw %>% 
    left_join(Climate, by = c("siteID" = "Site", "year" = "Year", "summerTemperature_gridded", "annualPrecipitation_gridded"))
  
  # Join fertility and trait data
  fertile_raw <- fertile_raw %>% 
    left_join(traits, by = "species")
  
  ### Data curation  
  fertile <- fertile_raw %>% 
    filter(year != 2010) %>% # remove 2010, only 1 plot (506)
    # remove species that occur in less than 3 years
    group_by(turfID, species) %>% 
    mutate(nYears = n()) %>%
    filter(nYears > 3) %>% 
    # remove shrubs etc.
    filter(functionalGroup %in% c("graminoid", "forb")) %>% 
    # relative fertility (correct for species having different proportion of fertility)
    group_by(species) %>% 
    mutate(mean.fertile = mean(PropFertile)) %>% 
    mutate(rel.fertile = PropFertile / mean.fertile) %>% 
    mutate(rel.fertile = ifelse(rel.fertile == "NaN", 0 , rel.fertile)) %>% 
    # Filter species (at site level) that never flower
    group_by(siteID, species) %>% 
    mutate(sum(SumOffertile)) %>% 
    filter(`sum(SumOffertile)` != 0) %>% 
    ungroup()
  
  return(fertile)
}


### SELECT COMMON SPECIES ###
CommonSpecies <- function(fertile){
  # Select common species (in time or space) that occur in 8 or more turfs
  commonSP <- fertile %>% 
    filter(NumberOfOccurrence > 20) %>% 
    ungroup() %>% 
    distinct(species)
  
  fertileCommon <- fertile %>% 
    inner_join(commonSP, by = "species") %>% 
    group_by(year, species) %>% 
    mutate(n = n()) %>% 
    filter(n > 7)
}

