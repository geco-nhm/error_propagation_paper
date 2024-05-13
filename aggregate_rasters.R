#Import libraries
library(raster)
library(terra)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Turn on progress bar
rasterOptions(progress = 'text', timer = TRUE)

#Define output SAGA grid file path
#file_paths <- list.files("data/raw/features/tif/", pattern = ".tif$",full.names = T)
file_paths <- list.files("data/raw/features/tif_aggregated/", pattern = ".tif$",full.names = T)

#Import data
raster_stack <- stack(file_paths)

#Define function for aggregating rasters
aggregate.rasters <- function(raster_input, aggregation_factor, resolution_level) {
  
  #Aggregate with median as focal statistic
  raster_stack_med <- aggregate(raster_input, fact = aggregation_factor, fun = "median", cores = 8, expand = F, na.rm = T)
  
  #Save files
  for (i in 1:nlayers(raster_stack_med)) {
    writeRaster(raster_stack_med[[i]], paste("data/processed/features/",resolution_level,"/",names(raster_stack_med[[i]]),"_med.tif", sep = ""), overwrite = TRUE)
  }
  
  #Aggregate with standard deviation as focal statistic
  raster_stack_sd <- aggregate(raster_input, fact = aggregation_factor, fun = "sd", cores = 8, expand = F, na.rm = T)
  
  #Save files
  for (i in 1:nlayers(raster_stack_sd)) {
    writeRaster(raster_stack_sd[[i]], paste("data/processed/features/",resolution_level,"/",names(raster_stack_sd[[i]]),"_sd.tif", sep = ""), overwrite = TRUE)
  }
}

#aggregate.rasters(raster_stack, 3, "high")
#aggregate.rasters(raster_stack, 2, "medium-high")
aggregate.rasters(raster_stack, 2, "low-medium")
#aggregate.rasters(raster_stack, 2, "low")