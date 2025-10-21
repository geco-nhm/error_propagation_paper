#Import libraries
library(readxl)
library(sf)
library(caret)

#Define function for calculating the mode
calculate.mode <- function(vector_object, map_scale) {
  
  #Order the vector based on experience 
  if (map_scale == "fine") {
    
    vector_object <- vector_object[c(1, 5, 4, 3, 6, 2)]
  } else {
    
    vector_object <- vector_object[c(4, 3, 1, 6, 2, 1)]
  }
  
  #Identify the classes for each data point
  unique_values <- apply(vector_object, 1, unique)
  
  #Identify the frequency of which each type was mapped
  frequency_values <- apply(vector_object, 1, table)
  
  #Sort them in decreasing order
  sorted_list <- lapply(frequency_values, function(x) sort(x, decreasing = TRUE))
  
  #Check if there were ties
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
          
          #Storing the ranked experience in each group
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

#Define function to compute ecological distance
compute.ED <- function(x, test_data, column, test_predictions) {
  conversion_list[[which(hierarchical_level == column)]][test_predictions[x],test_data[,column][x]]
}

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_list <- list()

#Import files with class names and store them in a list
for (i in 1:length(file_paths)) {
  conversion_list[[i]] <- as.data.frame(readxl::read_xlsx(file_paths[i]))
}

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_schemes <- list()

#Import files with class names and store them in a list
for (i in 1:length(file_paths)) {
  conversion_schemes[[i]] <- colnames(as.data.frame(readxl::read_xlsx(file_paths[i])))
}

#Save data frame
validation_distance <- read.csv("data/processed/test/test_data_distance.csv")

#Import test data
test_data <- read.csv("data/processed/test/test_data.csv")[-c(1)]

#Specify column names
test_labels <- colnames(test_data)[1:4]

#Convert test labels from numeric to factor
for (i in test_labels) {
  test_data[,i] <- as.factor(test_data[,i])
}

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

#Import data
vector_layer <- st_read(dsn = "data/raw/test/", layer = "consensus")

#Reproject data
vector_layer <- st_transform(vector_layer, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

#Obtain coordinates for the vector
coords <- st_coordinates(vector_layer)

#Create list for storing interpreter matrices
interpreter_matrices <- list()

#Find interpreter results for test data points
for (i in 1:length(interpreters)) {
  
  #Find intersection between interpreter polygons and test points
  intersection <- st_intersection(interpreter_list[[i]], vector_layer)
  
  #Select columns with classes
  interpreter_matrices[[i]] <- as.data.frame(intersection[,c("gtype1","htype1","htypegr1")])[,c(1,2,3)]
  
  #Reset the row names
  rownames(interpreter_matrices[[i]]) <- NULL
  
  #Add interpreter to column names
  colnames(interpreter_matrices[[i]]) <- paste(colnames(interpreter_matrices[[i]]),"_",interpreters[i], sep = "")
}

#Bind interpreter matrices
interpreter_matrix <- do.call(cbind, interpreter_matrices)

#Specify hierarchical levels
hierarchical_levels <- c("gtype1","htype1","htypegr1")


#Specify interpreters with 1:5000 maps
fine <- c("A5","F5","D5","C5","B5","E5","X5")

#Specify interpreters with 1:20000 maps
coarse <- c("I20","K20","H20","G20","L20","J20","X20")

#Create mode classification for the hierarchical levels in each block
for (j in 1:length(hierarchical_levels)) {
  
  #Specify hierarchical level
  columns <- interpreter_matrix[c(grepl(hierarchical_levels[j], colnames(interpreter_matrix)))]
    
  #Select 1:5000 labels    
  data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% fine]
    
  #Calculate the mode of all labels for each observation    
  interpreter_matrix$mode_column <- calculate.mode(data_frame, "fine")
    
  #Rename the column
  colnames(interpreter_matrix)[ncol(interpreter_matrix)] <- paste(hierarchical_levels[j],"_X5", sep = "")
    
  #Select 1:20 000 labels
  data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% coarse]
    
  #Calculate the mode of all labels for each observation
  interpreter_matrix$mode_column <- calculate.mode(data_frame, "coarse")
    
  #Rename the column
  colnames(interpreter_matrix)[ncol(interpreter_matrix)] <- paste(hierarchical_levels[j],"_X20", sep = "")
}

#Specify hierarchical levels
hierarchical_levels <- c("gtype1_20","gtype1_5","htype1","htypegr1")

#Convert column names depending on map scale and convert character labels to integer values
for (i in 1:length(hierarchical_levels)) {
  
  #Convert column name for 1:5000 interpreters
  interpreter_matrix <- conversion.classes(interpreter_matrix, fine, "5")
    
  #Convert column name for 1:20000 interpreters
  interpreter_matrix <- conversion.classes(interpreter_matrix, coarse, "20")
    
  #Specify vector with conversion key
  conversion_scheme <- conversion_schemes[[i]]
  
  #Specify columns that will be converted
  columns <- grepl(hierarchical_levels[[i]],colnames(interpreter_matrix))
    
  #Apply function to convert from character to integer values
  converted_labels <- apply(interpreter_matrix[,columns], 2, convert.labels, conversion_scheme)
    
  #Convert to data frame
  interpreter_matrix[,columns] <- as.data.frame(converted_labels)
}

#Specify hierarchical levels
hierarchical_level <- c("gtype1_20","gtype1_5","htype1","htypegr1")

#Specify hierarchical levels
hierarchical_levels <- c("gtype1","htype1","htypegr1")

#Specify interpreters
interpreters <- c("A5","F5","D5","C5","B5","E5","X5","I20","K20","H20","G20","L20","J20","X20")

#Create matrix for storing results
output_matrix <- expand.grid(interpreter = interpreters,
                             hierarchicallevel = hierarchical_levels)

#Create column for map scale
output_matrix$mapscale <- ifelse(output_matrix$interpreter %in% c("A5","F5","D5","C5","B5","E5","X5"), 5, 20)

#Create column for interpreter error
output_matrix$interpreter_error <- NA

#Create column for interpreter ecological distance
output_matrix$interpreter_ED <- NA

#Create column for number of classes
output_matrix$interpreter_classes <- NA

#Create data frame for storing spatial error results
spatial_error <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create data frame for storing spatial ecological distance results
spatial_ED <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create data frame for storing validation distance
spatial_distance <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create list for storing confusion matrices
confusion_list <- list()

#Specify start row
start_row <- 1

#Specify end row
end_row <- nrow(output_matrix)

#Generate results for each interpreter
for (i in start_row:end_row) {
  
  #Specify interpreter
  interpreter <- output_matrix$interpreter[i]
  
  #Specify hierarchical level
  hierarchicallevel <- output_matrix$hierarchicallevel[i]
  
  #Specify map scale
  mapscale <- output_matrix$mapscale[i]
  
  #Select column based on interpreter, map scale, and hierarchical level
  column_name <- paste(hierarchicallevel,"_",interpreter, sep = "")
  if(hierarchicallevel == "gtype1"){
    if(mapscale == 5){
      column_name <- paste(hierarchicallevel,"_5_",interpreter, sep = "")
      test <- as.factor(test_data[,c("gtype1_5")])
    }
    else {
      column_name <- paste(hierarchicallevel,"_20_",interpreter, sep = "")
      test <- as.factor(test_data[,c("gtype1_20")])
    }
  } else {
    test <- as.factor(test_data[,as.character(hierarchicallevel)])
  }
  
  #Convert the vector from numeric to factor
  interpreter_vector <- as.factor(interpreter_matrix[,column_name])
  
  #Harmonize the classes in the test data and model predictions and opposite
  test <- factor(test, unique(c(levels(interpreter_vector), levels(test))))
  interpreter_vector <- factor(interpreter_vector, unique(c(levels(interpreter_vector), levels(test))))
  
  #Compute error
  output_matrix$interpreter_error[i] <- 1-(sum(interpreter_vector == test)/length(interpreter_vector))
  
  #Generate the confusion matrix
  confusion_matrix <- confusionMatrix(interpreter_vector, test)$table
  
  #Store the confusion matrix in a list
  confusion_list[[i]] <- confusion_matrix
  
  #Specify column (interpreter, map scale, and hierarchical level)
  test_column <- gsub(paste("_",interpreter, sep = ""), "", column_name)
  
  #Specify the relevant ecological distance matrix
  ED_matrix <- conversion_list[[which(hierarchical_level == test_column)]][as.numeric(colnames(confusion_matrix)),as.numeric(colnames(confusion_matrix))]
  
  #Compute ecological distance
  output_matrix$interpreter_ED[i] <- sum(confusion_matrix*ED_matrix)/sum(confusion_matrix)
  
  #Store the number of classes
  output_matrix$interpreter_classes[i] <- length(unique(interpreter_vector))
  
  #Compute error for each test point
  spatial_error[i,] <- as.numeric(interpreter_vector == test)
  
  #Compute ecological distance for each test point
  spatial_ED[i,] <- sapply(1:length(test), compute.ED, test_data, test_column, interpreter_vector)
  
  #Insert validation distance
  spatial_distance[i,] <- validation_distance[, paste0(interpreter, "_dist")]
}

#Save files
write.csv(output_matrix, "results_ijrs/interpreter_results.csv")
write.csv(spatial_error, "results_ijrs/spatial_interpreter_error.csv")
write.csv(spatial_distance, "results_ijrs/spatial_interpreter_distance2.csv")
write.csv(spatial_ED, "results_ijrs/spatial_interpreter_ED.csv")

#Save confusion matrices
saveRDS(confusion_list, "results_ijrs/interpreter_confusion.rds")
