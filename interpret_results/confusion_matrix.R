#Import libraries
library(readxl)
library(flextable)
library(dplyr)
library(tibble)
library(officer)

#Set random seed
set.seed(123)

#Define function that round a number to one if between one and zero
round.number <- function(x) {
  if (x == 0) {
    return(0)
  } else if (x > 0 && x < 1) {
    return(1)
  } else {
    return(round(x))
  }
}

#Function to aggregate confusion matrices
confusion.matrix <- function(spatial_scale, hierarchical_level, ED_matrix, confusion_list, data, progress = TRUE, filename) {
  
  #Create matrix for storing confusion matrices
  confusion_matrix <- matrix()
  
  #Specify hierarchical level specific rows 
  hierarchicallevel <- which(data$hierarchicallevel == hierarchical_level)
  
  #If the desired hierarchical level is the minor type level, specify map scale specific rows  
  if(hierarchical_level == "gtype1") {
    spatiallevel <- which(data$mapscale == spatial_scale)
    hierarchicallevel <- hierarchicallevel[hierarchicallevel %in% spatiallevel]
  }
  
  #Create array for storing confusion matrices
  confusion_array <- array(matrix(0,ncol(ED_matrix),ncol(ED_matrix)), dim = c(ncol(ED_matrix),ncol(ED_matrix),length(hierarchicallevel)))
  
  #Aggregate confusion matrices within the hierarchical level
  for (k in 1:length(hierarchicallevel)) {
    
    #Create template with all classes for storing confusion matrix results
    confusion_template <- matrix(0,ncol(ED_matrix),ncol(ED_matrix))
    
    #Store confusion matrix k
    confusion_matrix <- confusion_list[[hierarchicallevel[k]]]
    
    #Store confusion matrix from one model in the template
    for (i in 1:ncol(confusion_matrix)) {
      for (j in 1:nrow(confusion_matrix)) {
        
        #Add values from confusion matrix k to the template
        confusion_template[as.numeric(rownames(confusion_matrix)[j]),as.numeric(colnames(confusion_matrix)[i])] <- confusion_template[as.numeric(rownames(confusion_matrix)[j]),as.numeric(colnames(confusion_matrix)[i])] + confusion_matrix[j,i]
      }
    }
    
    #Add results from confusion matrix k to the array
    confusion_array[,,k] <- confusion_template
    
    #Print progress
    if (progress == T) {
      print(k/length(hierarchicallevel))
    }
  }
  
  #Store the number of test points for each class
  test_numbers <- colSums(confusion_template)
  
  #Store the total number of test points
  total_number <- sum(confusion_template)
  
  #Aggregate the confusion matrices with the mean
  mean_confusion_matrix <- apply(confusion_array, c(1,2), mean)
  
  #Convert to data frame and convert to columns and rows
  mean_confusion_matrix <- convert.names(mean_confusion_matrix, ED_matrix)
  
  #Aggregate the confusion matrices with the standard deviation
  sd_confusion_matrix <- apply(confusion_array, c(1,2), sd)
  
  #Convert to data frame and convert to columns and rows
  sd_confusion_matrix <- convert.names(sd_confusion_matrix, ED_matrix)
  
  #Identify rows that will be removed
  nonzero <- sort(unique(c(which(rowSums(mean_confusion_matrix) != 0), which(colSums(mean_confusion_matrix) != 0))), decreasing = F)
  
  #Specify number of test points for all classes in the matrix
  test_numbers <- test_numbers[nonzero]
  
  #Remove zero columns
  mean_confusion_matrix <- mean_confusion_matrix[nonzero,nonzero]
  sd_confusion_matrix <- sd_confusion_matrix[nonzero,nonzero]
  
  #Assign the number of tested points to the name for each class
  colnames(mean_confusion_matrix) <- paste(colnames(mean_confusion_matrix)," (",test_numbers,")", sep = "")

  #Calculate user's accuracy
  users_accuracy <- diag(as.matrix(mean_confusion_matrix))/rowSums(mean_confusion_matrix)
  users_accuracy[is.nan(users_accuracy)] <- NA
  
  #Store empty rows
  empty_rows <- which(rowSums(mean_confusion_matrix) == 0)
  
  #Calcuate producer's accuracy
  producers_accuracy <- diag(as.matrix(mean_confusion_matrix))/colSums(mean_confusion_matrix)
  producers_accuracy[is.nan(producers_accuracy)] <- NA
  
  #Store empty columns
  empty_columns <- which(colSums(mean_confusion_matrix) == 0)
  
  #Calculate overall Accuracy
  overall_accuracy <- sum(diag(as.matrix(mean_confusion_matrix)))/sum(mean_confusion_matrix)
  
  #Create a matrix to store concatenated matrices
  concatenated_matrix <- matrix("", nrow = nrow(mean_confusion_matrix), ncol = ncol(mean_confusion_matrix))
  
  #Convert from numeric to character and concatenate matrices cell-wise
  for (i in 1:nrow(mean_confusion_matrix)) {
    for (j in 1:ncol(mean_confusion_matrix)) {
      concatenated_matrix[i,j] <- paste(as.character(mean_confusion_matrix[i,j]),"\u00B1", as.character(sd_confusion_matrix[i,j]), sep = "")
    }
  }
  
  #Convert to data frame
  concatenated_matrix <- as.data.frame(concatenated_matrix)
  
  #Rename columns
  colnames(concatenated_matrix) <- colnames(mean_confusion_matrix)
  
  #Rename rows
  rownames(concatenated_matrix) <- rownames(mean_confusion_matrix)
  
  #Designate zeros with hyphens
  concatenated_matrix[which(mean_confusion_matrix == 0, arr.ind = T)] <- "-"
  
  #Add user's accuracy to data frame
  concatenated_matrix <- rbind(concatenated_matrix, t(data.frame(Total = round(producers_accuracy, digits = 2))))
  
  #Add producer's accuracy and overall accuracy to data frame
  concatenated_matrix <- cbind(concatenated_matrix, data.frame(Total = round(c(users_accuracy,overall_accuracy), digits = 2)))
  
  #Rename the total column
  colnames(concatenated_matrix)[ncol(concatenated_matrix)] <- paste("Total"," (",total_number,")", sep = "")
  #rownames(concatenated_matrix)[nrow(concatenated_matrix)] <- paste("Total"," (",sum(rowSums(mean_confusion_matrix)),")", sep = "")
  
  #Rename NA cells
  concatenated_matrix[which(is.na(concatenated_matrix), arr.ind = TRUE)] <- "-"
  
  #Remove empty rows
  concatenated_matrix[empty_rows,ncol(concatenated_matrix)] <- "-"
  
  #Remove empty columns
  concatenated_matrix[nrow(concatenated_matrix),empty_columns] <- "-"
  
  #Create a table
  confusion_table <- flextable(concatenated_matrix %>% rownames_to_column(" "))
  
  #Specify the diagonal indices
  diagonal_indices <- 1:nrow(concatenated_matrix)
  
  #Add bold text to the diagonal of the table
  for (i in 1:length(diagonal_indices)) {
    confusion_table <- bold(confusion_table, i = diagonal_indices[i], j = diagonal_indices[i]+1, bold = TRUE, part = "body")
  }
  
  #Create a Word document
  document <- read_docx()
  
  #Add the table to a Word document
  document <- document %>%
    body_add_flextable(value = confusion_table)
  
  #Save the Word document
  print(document, target = paste("results/",filename,".docx", sep = ""))
  
  #Return the table
  return(confusion_table)
}

#Define function that converts to data frame and renames columns and rows
convert.names <- function(data_frame, ED_matrix) {
  
  #Round off to two digits
  data_frame <- round(data_frame, digits = 1)
  
  #Convert to data frame
  data_frame <- as.data.frame(data_frame)
  
  #Rename columns
  colnames(data_frame) <- colnames(ED_matrix)
  
  #Rename rows
  rownames(data_frame) <- colnames(ED_matrix)
  
  #Return the data frame
  return(data_frame)
}

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

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_list <- list()

#Import files with class names and store them in list
for (i in 1:length(file_paths)) {
  conversion_list[[i]] <- as.data.frame(readxl::read_xlsx(file_paths[i]))
}

#Import confusion matrices for classifiers
classifier_confusion <- readRDS("results_ijrs/classifier_confusion2.rds")

#Import confusion matrices for interpreters
interpreter_confusion <- readRDS("results_ijrs/interpreter_confusion.rds")

#Import model results
data <- read.csv("results_ijrs/results.csv")[,-c(1)]

#Import interpreter results
interpreter_data <- read.csv("results_ijrs/interpreter_results.csv")[,-c(1)]

#Specify levels
level_scale <- c("gtype1_20","gtype1_5","htype1","htypegr1")
level_hierarchical <- c("gtype1","gtype1","htype1","htypegr1")
level_spatial <- c(20,5,NA,NA)

#Aggregate and save confusion matrices for classifiers and interpreters
for (i in 3) {
  
  #Aggregate confusion matrices for classifiers
  aggregated_table <- confusion.matrix(level_spatial[i], level_hierarchical[i], conversion_list[[i]], classifier_confusion, data, progress = TRUE, filename = paste("confusion_classifiers_",level_scale[i], sep = ""))
  
  #Aggregate confusion matrices for interpreters
  aggregated_table <- confusion.matrix(level_spatial[i], level_hierarchical[i], conversion_list[[i]], interpreter_confusion, interpreter_data, progress = TRUE, filename = paste("confusion_interpreters_",level_scale[i], sep = ""))
  
}