#Import libraries
library(sf)
library(readxl)
library(raster)
library(terra)

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_schemes <- list()

#Import files with class names and store them in list
for (i in 1:length(file_paths)) {
  conversion_schemes[[i]] <- colnames(as.data.frame(readxl::read_xlsx(file_paths[i])))
}

#Specify hierarchical levels
hierarchical_levels <- c("gtype1_20","gtype1_5","htype1","htypegr1")

#Specify spatial resolutions
spatial_resolution <- c("low","medium","high")

#Import data
vector_layer <- st_read(dsn = "data/raw/test/", layer = "consensus")

#Reproject data
vector_layer <- st_transform(vector_layer, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

#Obtain coordinates for the vector
coords <- st_coordinates(vector_layer)

#Create list for raster stacks
raster_list <- list()

#Import feature data
for (i in 1:length(spatial_resolution)) {
  
  #Specify file paths
  file_paths <- list.files(paste("data/processed/features/",spatial_resolution[i],sep = ""), pattern = "tif$", full.names = TRUE)
  
  #Import features
  raster_list[[i]] <- stack(file_paths)
}

#Obtain cell number for the vector layer
cell_number <- cellFromXY(raster_list[[3]][[1]], coords)

#Reclassify missing data
vector_layer$gtype1[is.na(vector_layer$gtype1)] <- "T38-C-1"
vector_layer$htypegr1[is.na(vector_layer$htypegr1)] <- "T"

#Rename column for 1:5000 classes
names(vector_layer)[11] <- "gtype1_5"

#Make an extra column for 1:20000 classes
vector_layer$gtype1_20 <- vector_layer$gtype1_5

#Convert 1:5000 to 1:20000 classes
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T1-C-2")] <- "T1-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T2-C-1")] <- "T2-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T4-C-1")] <- "T4-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T4-C-2")] <- "T4-E-3"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T4-C-5")] <- "T4-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T30-C-2")] <- "T30-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T31-C-2")] <- "T31-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T32-C-4")] <- "T32-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T32-C-6")] <- "T32-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T32-C-10")] <- "T32-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "T38-C-1")] <- "T38-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "V1-C-1")] <- "V1-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "V1-C-5")] <- "V1-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "V2-C-1")] <- "V2-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "V3-C-2")] <- "V3-E-1"
vector_layer$gtype1_20[which(vector_layer$gtype1_20 == "V12-C-1")] <- "V12-E-1"

#Create data frame of the vector
data_frame <- as.data.frame(vector_layer[,c("htypegr1","htype1","gtype1_5","gtype1_20")])[,-c(5,6)]

#Convert class codes to integer values
for (i in 1:length(hierarchical_levels)) {
  data_frame[,hierarchical_levels[i]] <- as.integer(factor(data_frame[,hierarchical_levels[i]], 
                                                            levels = conversion_schemes[[i]]))
}

#Create list for storing feature matrices
data_list <- list()

#Create feature matrices
for (i in 1:length(spatial_resolution)) {
  
  #Obtain cell number for the vector layer
  cell_number <- cellFromXY(raster_list[[i]], coords)
  
  #Extract feature data for vector layer
  data_list[[i]] <- data.frame(ID = cell_number, extract(raster_list[[i]], cell_number))
  
  #Rename the columns
  colnames(data_list[[i]]) <- paste(colnames(data_list[[i]]), "_", spatial_resolution[i], sep = "")
}

#Bind target and feature data
test_data <- cbind(data_frame, do.call(cbind, data_list))

#Save data frame
write.csv(test_data, "data/processed/test/test_data.csv")

#Removing irrelevant columns
vector_layer <- vector_layer[,c("fid","gtype1_5","gtype1_20","htype1","htypegr1")]

#Save vector layer
st_write(vector_layer, dsn = "data/processed/test/", layer = "test_points.shp", driver = "ESRI Shapefile", append = FALSE)