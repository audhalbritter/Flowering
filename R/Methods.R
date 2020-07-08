# Table 1

MakeTable1 <- function(sites){
  Table1 <- sites %>% 
    arrange(temperature_level, precipitation_level) %>% 
    select(siteID, latitude, longitude, elevation_masl, temperature_level2, precipitation_level2) %>% 
    rename("latitude_°N" = "latitude", "longitude_°E" = "longitude", "Temperature level" = "temperature_level2", "Precipitation level in mm" = "precipitation_level2")
  
  return(Table1)
}



# MAP
MakeMap <- function(sites){
  temp <- raster::getData('worldclim', var='bio', res = 0.5, lat = 61, lon = 6)
  e <- raster::extent(4.5 ,10 , 60, 62)
  temp.map <- raster::crop(temp, e)
  
  # To convert your RasterLayer to a data.frame, you need to convert it to
  # a SpatialPixelsDataFrame first
  temp.map.spdf <- as(temp.map, "SpatialPixelsDataFrame")
  temp.map.df <- as.data.frame(temp.map.spdf)
  
  #square = data.frame(x = c(102, 102, 102, 102),
  #y = c(29.92, 29.80, 29.80, 29.92))
  
  my_colors <- RColorBrewer::brewer.pal(5, "Blues")[2:5]
  
  Map <- ggplot() +
    geom_raster(data = temp.map.df, aes(x = x, y = y, fill = bio12_06)) +
    geom_point(data = sites, aes(x = longitude, y = latitude, colour = precipitation_level2, shape = temperature_level2), size = 5) +
    scale_x_continuous(expand = c(0,0), limits = c(4.5, 10)) +
    scale_y_continuous(expand = c(0,0), limits = c(60, 62)) +
    scale_colour_manual(name = "Precipitation", values = my_colors) +
    scale_shape_manual(name = "Temperature", values = c(17, 16, 15)) +
    coord_equal() +
    scale_fill_gradient(name = "Annual precipitation", low = "grey80", high = "grey0") +
    labs(x = "", y = "") +
    theme_minimal()
  
  return(Map)
}

MakeClimatePlot <- function(Climate, sites){
  ClimatePlot <- Climate %>% 
    mutate(Temp_level = case_when(Site %in% c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen") ~ "alpine",
                                  Site %in% c("Alrust", "Hogsete", "Rambera", "Veskre") ~ "subalpine",
                                  Site %in% c("Fauske", "Vikesland", "Arhelleren", "Ovstedal") ~ "boreal"),
           Prec_level = case_when(Site %in% c("Ulvhaugen", "Alrust", "Fauske") ~ "600mm",
                                  Site %in% c("Lavisdalen", "Hogsete", "Vikesland") ~ "1200mm",
                                  Site %in% c("Gudmedalen", "Arhelleren", "Rambera") ~ "2000mm",
                                  Site %in% c("Skjellingahaugen", "Veskre", "Ovstedal") ~ "2700mm")) %>% 
    mutate(Prec_level = factor(Prec_level, levels = c("600mm", "1200mm", "2000mm", "2700mm"))) %>% 
    ggplot(aes(x = MeanSummerTemp, y = AnnPrec, colour = factor(Year), size = AnnPrec)) +
    geom_point(alpha = 0.8) +
    scale_colour_viridis_d(option = "inferno", name = "Year") +
    scale_size_continuous(name = "Precipitation mm") +
    labs(x = "Mean summer temperature in °C", y = "Annual precipitation in mm") +
    facet_grid(Temp_level ~ Prec_level) +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(ClimatePlot)
}



