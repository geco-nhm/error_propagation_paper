#Import libraries
library(rgdal)
library(raster)
library(terra)
library(rgeos)
library(sp)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import mask
vector_mask <- readOGR(dsn = "data/raw/target", layer = "AB")

#Specify file path
file_path <- list.files("data/raw/features/dtm/", pattern = "tif$", full.names = TRUE)

#Import data
mask_layer <- raster(file_path)

#Reproject raster
mask_layer <- projectRaster(mask_layer, crs = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))

#Change resolution
res(mask_layer) <- c(0.1,0.1)

#Make mask layer
mask_layer[] <- 1

#Crop and adapt raster to mask layer
mask_layer <- crop(mask_layer, extent(vector_mask)+100)

# Mask the raster with the buffered polygons
mask_layer <- mask(mask_layer, gBuffer(vector_mask, width = 10))

#Aggregate cells to 1.5 m resolution
mask_layer <- aggregate(mask_layer, fact = 5)

#Save to disk
writeRaster(mask_layer, "data/raw/features/mask/mask.tif", overwrite = T)

#Plot mask
plot(mask_layer)
plot(vector_mask, add = T, border = "red", col = "transparent")
