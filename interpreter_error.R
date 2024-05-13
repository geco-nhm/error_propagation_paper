#Import libraries
library(readxl)
library(sf)
library(caret)

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

#Define functions
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
  conversion_list[[i]] <- as.data.frame(read_xlsx(file_paths[i]))
}

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_schemes <- list()

#Import files with class names and store them in a list
for (i in 1:length(file_paths)) {
  conversion_schemes[[i]] <- colnames(as.data.frame(read_xlsx(file_paths[i])))
}

#Import test data
test_data <- read.csv("data/processed/test/test_data.csv")[-c(1)]

#Specify column names
test_labels <- colnames(test_data)[1:4]

#Convert test labels from numeric to factor
for (i in test_labels) {
  test_data[,i] <- as.factor(test_data[,i])
}

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
fine <- c("AB","AEN","EAF","EL","RH","TS","mode5")

#Specify interpreters with 1:20000 maps
coarse <- c("AKW","CO","ESV","HB","MT","PH","mode20")

#Create mode classification for the hierarchical levels in each block
for (j in 1:length(hierarchical_levels)) {
  
  #Specify hierarchical level
  columns <- interpreter_matrix[c(grepl(hierarchical_levels[j], colnames(interpreter_matrix)))]
    
  #Select 1:5000 labels    
  data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% fine]
    
  #Calculate the mode of all labels for each observation    
  interpreter_matrix$mode_column <- apply(data_frame,1,calculate.mode, "fine")
    
  #Rename the column
  colnames(interpreter_matrix)[ncol(interpreter_matrix)] <- paste(hierarchical_levels[j],"_mode5", sep = "")
    
  #Select 1:20 000 labels
  data_frame <- columns[,sub(paste(hierarchical_levels[j],"_", sep = ""),"",colnames(columns)) %in% coarse]
    
  #Calculate the mode of all labels for each observation
  interpreter_matrix$mode_column <- apply(data_frame,1,calculate.mode, "coarse")
    
  #Rename the column
  colnames(interpreter_matrix)[ncol(interpreter_matrix)] <- paste(hierarchical_levels[j],"_mode20", sep = "")
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
interpreters <- c("AB","AEN","EAF","EL","RH","TS","mode5","AKW","CO","ESV","HB","MT","PH","mode20")

#Create matrix for storing results
output_matrix <- expand.grid(interpreter = interpreters,
                             hierarchicallevel = hierarchical_levels)

#Create column for map scale
output_matrix$mapscale <- ifelse(output_matrix$interpreter %in% c("AB","AEN","EAF","EL","RH","TS","mode5"), 5, 20)

#Create column for interpreter error
output_matrix$interpreter_error <- NA

#Create column for interpreter ecological distance
output_matrix$interpreter_ED <- NA

#Create data frame for storing spatial error results
spatial_error <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create data frame for storing spatial ecological distance results
spatial_ED <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create list for storing confusion matrices
confusion_list <- list()

#Generate results for each interpreter
for (i in 1:nrow(output_matrix)) {
  
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
  
  #Compute error for each test point
  spatial_error[i,] <- as.numeric(interpreter_vector == test)
  
  #Compute ecological distance for each test point
  spatial_ED[i,] <- sapply(1:length(test), compute.ED, test_data, test_column, interpreter_vector)
}

#Save files
write.csv(output_matrix, "results/interpreter_results.csv")
write.csv(spatial_error, "results/spatial_interpreter_error.csv")
write.csv(spatial_ED, "results/spatial_interpreter_ED.csv")

#Save confusion matrices
saveRDS(confusion_list, "results/interpreter_confusion.rds")