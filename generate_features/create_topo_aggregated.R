#Import libraries
library(RSAGA)
library(raster)
library(terra)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Specify SAGA environment
saga_env <- rsaga.env(path = "C:/SAGA-GIS/saga-8.4.1_x64/", cores = 8, parallel = T)

#Turn on progress bar
rasterOptions(progress = 'text', timer = TRUE)

#Import DEM
dem_tif <- raster("data/raw/features/tif/dem.tif")

#Set aggregation factor
aggregation_factor <- 6

#Aggregate DEM
dem_tif <- aggregate(dem_tif, fact = aggregation_factor, fun = "median", cores = 8, expand = F, na.rm = T)

#Import nDSM
ndsm_tif <- raster("data/raw/features/tif/ndsm.tif")

#Aggregate nDSM
ndsm_tif <- aggregate(ndsm_tif, fact = aggregation_factor, fun = "median", cores = 8, expand = F, na.rm = T)

#Define output SAGA grid file path
output_file <- "data/raw/features/sgrd_aggregated/dem.sgrd"

#Save as SGRD
writeRaster(dem_tif, output_file, format = "SAGA", overwrite = T)

#Define output SAGA grid file path
output_file <- "data/raw/features/sgrd_aggregated/ndsm.sgrd"

#Save as SGRD
writeRaster(ndsm_tif, output_file, format = "SAGA", overwrite = T)


#Set path to SAGA grid input files
input_dem <- "data/raw/features/sgrd_aggregated/dem.sgrd"
input_ndsm <- "data/raw/features/sgrd_aggregated/ndsm.sgrd"

#Set paths to output file
output_file <- "data/raw/features/sgrd_aggregated/sol_ndsm.sgrd"

#Calculate solar duration
rsaga.pisr2(input_dem,
            in.svf.grid = input_ndsm,
            out.total.grid = output_file,
            latitude = 60.5,
            unit = "kWh/m2",
            time.range = c(0,24),
            time.step = 1,
            start.date = list(day = 1, month = 6, year = 2023),
            end.date = list(day = 1, month = 8, year = 2023),
            day.step = 5,
            env = saga_env)

#Set path output file
output_file <- "data/raw/features/sgrd_aggregated/sol_dem.sgrd"

#Calculate solar duration
rsaga.pisr2(input_dem,
            out.total.grid = output_file,
            latitude = 60.5,
            unit = "kWh/m2",
            time.range = c(0,24),
            time.step = 1,
            start.date = list(day = 1, month = 6, year = 2023),
            end.date = list(day = 1, month = 8, year = 2023),
            day.step = 5, # Or 73 if slow
            env = saga_env)

#Define output SAGA grid file path
output_file <- "data/raw/features/sgrd_aggregated/twi.sgrd"

#Calculate topographic wetness
rsaga.wetness.index(input_dem, output_file, env = saga_env)

#Define output SAGA grid file path
output_slp <- "data/raw/features/sgrd_aggregated/slp.sgrd"
output_curv <- "data/raw/features/sgrd_aggregated/curv.sgrd"

#Calculate topographic wetness
rsaga.slope.asp.curv(input_dem,
                     out.slope = output_slp,
                     out.ctota = output_curv,
                     env = saga_env)

#Import DEM
raster_layer <- dem_tif

#Calculate TPI and TRI
raster_terrain <- terrain(raster_layer, opt = c("TPI","TRI"))

#Clamp TPI values (did not clamp two last levels)
#raster_terrain$tpi <- clamp(raster_terrain$tpi, lower = -1, upper = 1)

#Define output SAGA grid file path
sgrd_paths <- list.files("data/raw/features/sgrd_aggregated/", pattern = ".sdat$",full.names = T)[-c(2,3)] #Check this

#Import data
raster_stack <- stack(sgrd_paths,
                      raster_terrain)

#Specify coordinate reference system
proj4string(raster_stack) <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

#Clamp curvature values (did not clamp for two last levels)
#raster_stack$curv <- clamp(raster_stack$curv, upper = 10)

#Import DEM
mask_layer <- dem_tif

#Crop and adapt raster to mask layer
raster_stack <- mask(crop(raster_stack, mask_layer), mask_layer)


#Define output SAGA grid file path
file_paths <- list.files("data/raw/features/tif/", pattern = ".tif$",full.names = T)

#Import stack
extra_stack <- stack(file_paths[-c(2,11,12,13,14,15,16)])

#Aggregate stack
aggregated_stack <- aggregate(extra_stack, fact = aggregation_factor, fun = "median", cores = 8, expand = F, na.rm = T)

#Import data
extra_stack <- stack(raster_stack,
                     aggregated_stack)

#Save files
for (i in 1:nlayers(extra_stack)) {
  writeRaster(extra_stack[[i]], paste("data/raw/features/tif_aggregated/",names(extra_stack[[i]]),".tif", sep = ""), overwrite = TRUE)
}