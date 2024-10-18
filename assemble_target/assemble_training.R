#Import libraries
library(sf)
library(raster)
library(terra)
library(readxl)

#Define function for calculating the mode
calculate.mode <- function(vector_object, map_scale) {
  
  #Ordering the vector based on experience 
  if (map_scale == "fine") {
    
    #AB, RH, EL, EAF, TS, AEN
    vector_object <- vector_object[c(1, 5, 4, 3, 6, 2)]
  } else {
    
    #HB, ESV, AKW, PH, CO, MT
    vector_object <- vector_object[c(4, 3, 1, 6, 2, 1)]
  }
  
  #Identify the classes for each data point
  unique_values <- apply(vector_object, 1, unique)
  
  #Identify the frequency of which each type was mapped
  frequency_values <- apply(vector_object, 1, table)
  
  #Sort them in decreasing order
  sorted_list <- lapply(frequency_values, function(x) sort(x, decreasing = TRUE))
  
  #Check if there are ties
  ties <- lapply(sorted_list, function(x) return(any(x[-1] == x[1])))
  
  #Identify the elements with ties
  tied_elements <- which(unlist(ties))
  
  #Apply the function to each element in the list
  ties_count <- lapply(sorted_list, function(x) return(sum(x[-1] == x[1])))
  
  #Check how many groups are tied with the mode
  length_tie <- length(which(ties_count != 0))
  
  if (length_tie > 1) {
    
    #Create vector for storing the aggregated experience for the tied groups
    experience_sum <- list()
    
    tied_indices <- which(ties_count != 0)
    
    #Create vector for storing the index for the mode
    mode_index <- numeric()
    
    for (i in 1:length_tie) {
      
      tie_result <- sorted_list[tied_elements][[i]]
      
      match_tie <- match(vector_object[tied_elements,][i,],names(tie_result))
      
      sum_tie <- numeric()
      for (k in 1:length(tie_result)) {
        sum_tie[k] <- sum(which(match_tie == k))
      }
      experience_sum[[i]] <- sum_tie
      
      #Identify mode object
      mode_index[i] <- which.min(sum_tie)
      
      sorted_list[tied_elements][[i]] <- sorted_list[tied_elements][[i]][mode_index[i]]
      
      length_experience_tie <- list()
      
      #Check how many groups are tied with the minimum experience sum
      length_experience_tie[[i]] <- length(which(experience_sum[[i]] == min(experience_sum[[i]])))
      
      if (length_experience_tie[[i]] > 1) {
        
        #Create vector for storing the index of the group with the most experienced interpreter
        max_experience <- list()
        for (j in 1:length_experience_tie[[i]]) {
          
          #Store the ranked experience in each group
          max_experience[[i]] <- which.min(which(vector_object[tied_indices[[i]],] %in% unique_values[tied_indices[i]][[1]][1:(ties_count[[tied_indices[i]]]+1)]))
          
          names(sorted_list[tied_elements][[i]]) <- as.character(vector_object[tied_indices[[i]],][max_experience[[i]]])
        }
      }
    }
  }
  
  #Extract the first element from each element in the list
  mode <- unlist(sapply(sorted_list, function(x) names(x)[1]))
  
  
  mode <- as.character(mode)
  
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
mask_layer <- mask(raster_list[[1]][[1]], interpreter_list[[1]])

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
  print(length(within_block))
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
  conversion_schemes[[i]] <- colnames(as.data.frame(readxl::read_xlsx(file_paths[i])))
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
    target_matrices[[i]]$mode_column <- calculate.mode(data_frame, "fine")
    
    #Rename the column
    colnames(target_matrices[[i]])[ncol(target_matrices[[i]])] <- paste(hierarchical_levels[j],"_mode5", sep = "")
    
    #Select 1:20 000 labels
    data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% coarse]
    
    #Calculate the mode of all labels for each observation
    target_matrices[[i]]$mode_column <- calculate.mode(data_frame, "coarse")
    
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
