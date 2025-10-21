#Import libraries
library(raster)
library(terra)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Turn on progress bar
rasterOptions(progress = 'text', timer = TRUE)

#Import mask
mask_layer <- raster("data/raw/features/mask/mask.tif")

#Specify file paths
file_paths <- list.files("data/raw/features/multispectral/", pattern = "tif$", full.names = TRUE)

#Import data
raster_stack <- stack(file_paths)

#Reproject raster
raster_stack <- projectRaster(raster_stack, mask_layer, method = "bilinear")

#Crop and adapt raster to mask layer
raster_stack <- mask(crop(raster_stack, mask_layer), mask_layer)

#Save files
for (i in 1:nlayers(raster_stack)) {
  writeRaster(raster_stack[[i]], paste("data/raw/features/tif/",names(raster_stack[[i]]),".tif", sep = ""), overwrite = TRUE)
}