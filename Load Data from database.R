### LOAD FERTILITY DATA FROM DATABASE ####
library("RSQLite")
library("tidyverse")
#library("DBI")
library("RODBC")

con <- dbConnect(SQLite(), dbname = "Database/seedclim.sqlite")

# vies all subtables
DBI::dbListTables(conn = con)
# view column names of a table
dbListFields(con, "taxon")

#more traits = character_traits
#numeric_traits???
### FINAL QUIERY
# including vegetation height
fertilityQ <-"SELECT blocks.siteID, blocks.blockID, turfs.turfID, turfs.TTtreat, subTurfCommunity.year, subTurfCommunity.species, Count(subTurfCommunity.species) AS CountOfspecies, Sum(subTurfCommunity.fertile) AS SumOffertile, taxon.lifeSpan, sites.Temperature_level, sites.Precipitation_level, taxon.family, taxon.functionalGroup, [character_traits].[F-Vår], [character_traits].[F-Fso], [character_traits].[F-Mso], [character_traits].[F-Sso], [character_traits].[F-Hø], [character_traits].Nem, [character_traits].BNem, [character_traits].SBor, [character_traits].MBor, [character_traits].Nbor, [character_traits].LAlp, [character_traits].MAlp, [character_traits].HAlp, turfCommunity.cover, turfEnvironment.vegetationHeight
FROM ((sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON sites.siteID = blocks.siteID) INNER JOIN ((([character_traits] INNER JOIN (taxon INNER JOIN (subTurfCommunity INNER JOIN subTurfEnvironment ON (subTurfCommunity.subTurf = subTurfEnvironment.subTurf) AND (subTurfCommunity.year = subTurfEnvironment.year)) ON taxon.species = subTurfCommunity.species) ON [character_traits].species = subTurfCommunity.species) INNER JOIN turfCommunity ON (taxon.species = turfCommunity.species) AND (subTurfEnvironment.year = turfCommunity.year)) INNER JOIN turfEnvironment ON subTurfEnvironment.year = turfEnvironment.year) ON (turfs.turfID = turfEnvironment.turfID) AND (turfs.turfID = turfCommunity.turfID) AND (turfs.turfID = subTurfEnvironment.turfID) AND (turfs.turfID = subTurfCommunity.turfID)) INNER JOIN subTurfCommunity ON (subTurfCommunity.species = [character_traits].species) AND (subTurfCommunity.year = subTurfEnvironment.year) AND (subTurfCommunity.subTurf = subTurfEnvironment.subTurf) AND (turfs.turfID = subTurfCommunity.turfID) AND (taxon.species = subTurfCommunity.species)
GROUP BY blocks.siteID, blocks.blockID, turfs.turfID, turfs.TTtreat, subTurfCommunity.year, subTurfCommunity.species, taxon.lifeSpan, sites.Temperature_level, sites.Precipitation_level, taxon.family, taxon.functionalGroup, [character_traits].[F-Vår], [character_traits].[F-Fso], [character_traits].[F-Mso], [character_traits].[F-Sso], [character_traits].[F-Hø], [character_traits].Nem, [character_traits].BNem, [character_traits].SBor, [character_traits].MBor, [character_traits].Nbor, [character_traits].LAlp, [character_traits].MAlp, [character_traits].HAlp, turfCommunity.cover, turfEnvironment.vegetationHeight
HAVING (((turfs.TTtreat)<>' ') AND ((subTurfCommunity.year)<>2010) AND ((taxon.lifeSpan)<>'annual') AND ((taxon.functionalGroup)<>'woody' And (taxon.functionalGroup)<>'pteridophyte'));"

fertility <- tbl(con, sql(fertilityQ)) %>% 
  collect()



### This one copied from China data works
coverQ <-
  "SELECT sites.siteID AS originSiteID, blocks.blockID AS originBlockID, plots.plotID AS originPlotID, turfs.turfID, plots_1.plotID AS destPlotID, blocks_1.blockID AS destBlockID, sites_1.siteID AS destSiteID, turfs.TTtreat, turfCommunity.year, turfCommunity.species, turfCommunity.cover, turfCommunity.flag, taxon.speciesName
FROM blocks, sites, plots, turfs, turfCommunity, plots AS plots_1, blocks AS blocks_1, sites AS sites_1, taxon
WHERE blocks.siteID = sites.siteID AND plots.blockID = blocks.blockID AND turfs.originPlotID = plots.plotID AND turfCommunity.turfID = turfs.turfID AND turfs.destinationPlotID = plots_1.plotID AND blocks_1.siteID = sites_1.siteID AND plots_1.blockID = blocks_1.blockID AND turfCommunity.species = taxon.species"

cover.thin <- tbl(con, sql(coverQ)) %>% 
  collect()


