#Import libraries
library(raster)
library(terra)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Turn on progress bar
rasterOptions(progress = 'text', timer = TRUE)

#Import mask
mask_layer <- raster("data/raw/features/mask/mask.tif")

#Import DSM
raster_layer <- raster("data/raw/features/dsm.tif")

#Reproject
raster_layer <- projectRaster(raster_layer, mask_layer, method = "bilinear")

#Crop and adapt raster to mask layer
raster_layer <- mask(crop(raster_layer, mask_layer), mask_layer)

#Import mask
mask_layer <- raster("data/raw/features/tif/dem.tif")

#Normalize DSM
raster_layer <- raster_layer-mask_layer

#Clamp values below zero
raster_layer <- clamp(raster_layer, lower = 0)

#Specify file path
output_file <- "data/raw/features/tif/ndsm.tif"

#Save file
writeRaster(raster_layer, output_file, overwrite = TRUE)

#Specify file path
output_file <- "data/raw/features/tif/ndsm.sgrd"

#Save as SGRD
writeRaster(raster_layer, output_file, format = "SAGA")