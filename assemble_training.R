#Import libraries
library(sf)
library(raster)
library(terra)
library(readxl)

#Define function for calculating the mode
calculate.mode <- function(vector_object, map_scale) {
  
  #Ordering the vector based on experience 
  if(map_scale == "fine") {
    
    #AB, RH, EL, EAF, TS, AEN
    vector_object <- vector_object[c(1,5,4,3,6,2)]
  }
  else {
    
    #HB, ESV, AKW, PH, CO, MT
    vector_object <- vector_object[c(4,3,1,6,2,1)]
  }
  
  
  #Identify the different classes
  unique_values <- unique(as.character(vector_object))
  
  #Cacluate how often the different classes were selected
  frequency_values <- tabulate(match(vector_object, unique_values))
  
  #Identify mode object
  mode_index <- which.max(frequency_values)
  
  #Checking how many groups are tied with the mode
  length_tie <- length(which(frequency_values == frequency_values[1]))
  
  if(length_tie > 1) {
    
    #Create vector for storing the aggregated experience for the tied groups
    experience_sum <- numeric()
    for (i in 1:length_tie) {
      
      #Calculating the aggregated experience
      experience_sum <- c(experience_sum, sum(which(vector_object == unique_values[i])))
    }
    
    #Storing the index
    mode_index <- which.min(experience_sum)
    
    #Checking how many groups are tied with the mode
    length__experience_tie <- length(which(experience_sum == experience_sum[1]))
    
    if(length__experience_tie > 2) {
      
      #Create vector for storing the index of the group with the most experienced interpreter
      max_experience <- list()
      for (i in 1:length__experience_tie) {
        
        #Storing the ranked experience in each group
        max_experience[[i]] <- which(vector_object == unique_values[i])
        
        #Storing the index
        mode_index <- which.min(sapply(max_experience, min))
      }
    }
  }
  
  #Identify the mode class
  mode <- unique_values[mode_index]
  
  #Return the mode class
  return(mode)
}

#Define function for renaming columns depending on map scale
conversion.classes <- function(data, interpreter_scale, map_scale) {
  for (i in interpreter_scale) {
    #Rename columns by adding suffix depending on the map scale for the interpreter
    colnames(data)[grepl(paste("gtype1_",i,sep = ""), colnames(data))] <- paste("gtype1_",map_scale,"_",i,sep = "")
  }
  return(data)
}

#Define function for converting from character labels to integer values
convert.labels <- function(x, y){
  as.integer(factor(x, levels = y))
}

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Specify file names
interpreters <- c("AB","AEN","EAF","EL","RH","TS","AKW","CO","ESV","HB","MT","PH")

#Create list for storing vector layers
interpreter_list <- list()

#Import and reproject vector data
for (i in 1:length(interpreters)) {
  
  #Import vectors
  interpreter_list[[i]] <- st_read(dsn = "data/raw/target/", layer = interpreters[i])
  
  #Reproject vectors
  interpreter_list[[i]] <- st_transform(interpreter_list[[i]], crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
}

#Specify spatial resolutions
spatial_resolution <- c("low","medium","high")

#Create list for raster stacks
raster_list <- list()

#Import raster stacks
for (i in 1:length(spatial_resolution)) {
  
  #Specify file paths
  file_paths <- list.files(paste("data/processed/features/",spatial_resolution[i],sep = ""), pattern = "tif$", full.names = TRUE)
  
  #Store raster stacks in lists
  raster_list[[i]] <- stack(file_paths)
}

#Create mask
mask_layer <- mask(raster_list[[2]][[1]], interpreter_list[[1]])

#Rename mask layer
names(mask_layer) <- "mask"

#Identify cells within the ROI
within_roi <- which(!is.na(mask_layer[]))

#Convert values
mask_layer[within_roi] <- 1

#Import blocks
blocks <- st_read(dsn = "data/raw/blocks/", layer = "blocks")

#Reproject vectors
blocks <- st_transform(blocks, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

#Specify the maximum sample size
sample_size <- 3500

#Specify number of blocks
number_blocks <- 4

#Create list for storing block coordinates
coords_block <- list()

#Extract coordinates for each block
for (i in 1:number_blocks) {

  #Mask with block vector
  block_raster <- mask(mask_layer, blocks[i+1,])
  
  #Identify cells within the block
  within_block <- which(!is.na(block_raster[]))
  
  #Create a sample within the block 
  sample_block <- sample(within_block, sample_size, replace = FALSE)
  
  #Extract coordinates of the sampled cells
  coords_block[[i]] <- xyFromCell(mask_layer, sample_block)
}

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_schemes <- list()

#Import files with class names and store them in list
for (i in 1:length(file_paths)) {
  conversion_schemes[[i]] <- colnames(as.data.frame(read_xlsx(file_paths[i])))
}

#Specify hierarchical levels
hierarchical_levels <- c("gtype1","htype1","htypegr1")

#Specify interpreters with 1:5000 maps
fine <- c("AB","AEN","EAF","EL","RH","TS")

#Specify interpreters with 1:20000 maps
coarse <- c("AKW","CO","ESV","HB","MT","PH")

#Create list for storing labelled data
data_list <- list(list(),
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

#Assemble interpreter data for interpreters in each block
for (j in 1:length(coords_block)) {
for (i in 1:length(interpreters)) {
  
  #Specify coordinates for the block
  coords <- coords_block[[j]]
  
  #Specify interpreter (with buffer in case of small geometry ovelaps)
  vector_layer <- st_buffer(interpreter_list[[i]][,c("htypegr1","htype1","gtype1")], dist = 0)
  
  #Convert from sf to sp object
  vector_layer <- as(vector_layer, "Spatial")
  
  #Extract interpreter data from the block coordinates
  data_list[[i]][[j]] <- extract(vector_layer, coords)[,-c(1)]
  
  #Rename the columns
  colnames(data_list[[i]][[j]]) <- paste(colnames(data_list[[i]][[j]]),"_",interpreters[i], sep = "")
}
}

#Bind the nested lists together
target_matrices <- do.call(Map, c(cbind, data_list))


#Create mode classification for the hierarchical levels in each block
for (i in 1:length(target_matrices)) {
  for (j in 1:length(hierarchical_levels)) {
    
    #Specify hierarchical level
    columns <- target_matrices[[i]][c(grepl(hierarchical_levels[j], colnames(target_matrices[[i]])))]
    
    #Select 1:5000 labels
    data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% fine]
    
    #Calculate the mode of all labels for each observation
    target_matrices[[i]]$mode_column <- apply(data_frame,1,calculate.mode, "fine")
    
    #Rename the column
    colnames(target_matrices[[i]])[ncol(target_matrices[[i]])] <- paste(hierarchical_levels[j],"_mode5", sep = "")
    
    #Select 1:20 000 labels
    data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% coarse]
    
    #Calculate the mode of all labels for each observation
    target_matrices[[i]]$mode_column <- apply(data_frame,1,calculate.mode, "coarse")
    
    #Rename the column
    colnames(target_matrices[[i]])[ncol(target_matrices[[i]])] <- paste(hierarchical_levels[j],"_mode20", sep = "")
  }
}

#Specify file names
interpreters <- c("AB","AEN","EAF","EL","RH","TS","mode5","AKW","CO","ESV","HB","MT","PH","mode20")

#Specify interpreters with 1:5000 maps
fine <- c("AB","AEN","EAF","EL","RH","TS","mode5")

#Specify interpreters with 1:20000 maps
coarse <- c("AKW","CO","ESV","HB","MT","PH","mode20")

#Specify hierarchical levels
hierarchical_levels <- c("gtype1_20","gtype1_5","htype1","htypegr1")

#Convert column names depending on map scale and convert character labels to integer values
for (j in 1:length(target_matrices)) {
for (i in 1:length(hierarchical_levels)) {
  
  #Convert column name for 1:5000 interpreters
  target_matrices[[j]] <- conversion.classes(target_matrices[[j]], fine, "5")
  
  #Convert column name for 1:20000 interpreters
  target_matrices[[j]] <- conversion.classes(target_matrices[[j]], coarse, "20")
  
  #Specify vector with conversion key
  conversion_scheme <- conversion_schemes[[i]]
  
  #Specify columns that will be converted
  columns <- grepl(hierarchical_levels[[i]],colnames(target_matrices[[j]]))
  
  #Apply function to convert from character to integer values
  converted_labels <- apply(target_matrices[[j]][,columns], 2, convert.labels, conversion_scheme)
  
  #Convert to data frame
  target_matrices[[j]][,columns] <- as.data.frame(converted_labels)
}
}

#Create list for storing feature data
data_list <- list(list(),
                  list(),
                  list())

#Assemble feature data for spatial resolutions in each block
for (j in 1:length(coords_block)) {
  for (i in 1:length(spatial_resolution)) {
    
    #Specify coordinates for the block
    coords <- coords_block[[j]]
    
    #Extract cell number from block coordinates
    cell_number <- cellFromXY(raster_list[[i]], coords)
    
    #Extract feature data from the block coordinates
    data_list[[i]][[j]] <- data.frame(ID = cell_number, extract(raster_list[[i]], cell_number))
    
    #Rename the columns
    colnames(data_list[[i]][[j]]) <- paste(colnames(data_list[[i]][[j]]),"_",spatial_resolution[i], sep = "")
  }
}

#Bind the nested lists together
feature_matrices <- do.call(Map, c(cbind, data_list))

#Create list for storing training data blocks
training_list <- list()

#Bind matrices and save to disk
for (i in 1:length(coords_block)) {
  
  #Bind target and feature data
  training_list[[i]] <- cbind(target_matrices[[i]],feature_matrices[[i]])
  
  #Save file
  write.csv(training_list[[i]], paste("data/processed/training/training_data_",i,".csv", sep = ""))
}

#Create list for storing ensemble training data sets
ensemble_list <- list(c(1,2),
                      c(3,4),
                      c(1,2,3,4))

#Create ensemble data sets
for (i in 1:length(ensemble_list)) {
  
  #Combine blocks to create ensemble data sets
  training_data <- do.call(rbind, lapply(ensemble_list[[i]], function(x){training_list[[x]]}))
  
  #Save file
  write.csv(training_data, paste("data/processed/training/training_data_",paste(ensemble_list[[i]],collapse = ""),".csv", sep = ""))
}
