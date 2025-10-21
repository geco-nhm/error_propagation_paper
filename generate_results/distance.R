#Import libraries
library(sf)
library(readxl)
library(raster)
library(terra)

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import data
vector_layer <- st_read(dsn = "data/raw/test/", layer = "consensus")

#Reproject data
vector_layer <- st_transform(vector_layer, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

#Obtain coordinates for the vector
coords <- st_coordinates(vector_layer) %>% as.data.frame()

#Specify file names
interpreters <- c("A5","F5","D5","C5","B5","E5","I20","K20","H20","G20","L20","J20")

#Create list for storing vector layers
interpreter_list <- list()

#Import and reproject vector data
for (i in 1:length(interpreters)) {
  
  #Import vectors
  interpreter_list[[i]] <- st_read(dsn = "data/raw/target/", layer = interpreters[i])
  
  #Reproject vectors
  interpreter_list[[i]] <- st_transform(interpreter_list[[i]], crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
}

#Create list for storing distance data
distance_list <- list(list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list(),
                      list())

#Set names on consensus maps
consensus <- c("X5","X20")

#Create list for storing vector layers
consensus_list <- list()

#Import and reproject vector data
for (i in 1:length(consensus)) {
  
  #Import vectors
  consensus_list[[i]] <- st_read(dsn = "consensus/", layer = consensus[i])
  
  #Reproject vectors
  consensus_list[[i]] <- st_transform(consensus_list[[i]], crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
}

#Bind consensus and interpreters
interpreter_list[[13]] <- consensus_list[[1]]
interpreter_list[[14]] <- consensus_list[[2]]
names(interpreter_list) <- c("A5","F5","D5","C5","B5","E5","I20","K20","H20","G20","L20","J20","X5","X20")

interpreters2 <- c(interpreters, consensus)

for (i in 1:length(interpreters2)) {
  
  #Select interpreter
  current_interpreter <- interpreter_list[[interpreters2[i]]]
  
  #Convert to sf point object
  points_sf <- st_as_sf(coords, coords = c("X", "Y"), crs = st_crs(current_interpreter))
  
  #Get the polygon boundaries only
  polygon_boundaries <- st_boundary(current_interpreter)
  
  #Compute distances (returns a matrix [n_points x n_polygons])
  dist_matrix <- st_distance(points_sf, polygon_boundaries)
  
  #Get the minimum distance for each point to any polygon
  min_distances <- apply(dist_matrix, 1, min)
  
  #Combine with points if needed
  result <- cbind(coords, as.numeric(min_distances))
  
  #Rename column
  colnames(result) <- c("x", "y", paste0(interpreters2[i],"_dist"))
  
  #Extract feature data from the block coordinates
  distance_list[[i]] <- result
  
  print(i/length(interpreters2))
  
}



#Bind the nested lists together
distance_matrices <- do.call(cbind, distance_list)

distance_matrices <- distance_matrices[,grep(pattern = "dist", colnames(distance_matrices))]

#Save data frame
write.csv(distance_matrices, "data/processed/test/test_data_distance.csv")