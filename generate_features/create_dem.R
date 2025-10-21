#Import libraries
library(raster)
library(terra)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Turn on progress bar
rasterOptions(progress = 'text', timer = TRUE)

#Import mask
mask_layer <- raster("data/raw/features/mask/mask.tif")

#Import DEM
raster_layer <- raster("data/raw/features/dtm/dtm.tif")

#Reproject raster
raster_layer <- projectRaster(raster_layer, crs = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))

#Resample raster
raster_layer <- resample(raster_layer, mask_layer, method = "bilinear")

#Crop and adapt raster to mask layer
raster_layer <- mask(crop(raster_layer, mask_layer), mask_layer)

#Specify file path
output_file <- "data/raw/features/tif/dem.tif"

#Save file
writeRaster(raster_layer, output_file, overwrite = TRUE)

#Define output SAGA grid file path
output_file <- "data/raw/features/sgrd/dem.sgrd"

#Save as SGRD
writeRaster(raster_layer, output_file, format = "SAGA")