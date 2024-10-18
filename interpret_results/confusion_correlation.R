#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Function to aggregate confusion matrices
confusion.matrix <- function(ED_matrix, confusion_list, elements, progress) {
  
  #Create matrix for storing confusion matrices
  confusion_matrix <- matrix()
  
  #Create array for storing confusion matrices
  confusion_array <- array(matrix(0,ncol(ED_matrix),ncol(ED_matrix)), dim = c(ncol(ED_matrix),ncol(ED_matrix),length(elements)))
  
  #Aggregate confusion matrices within the hierarchical level
  for (k in 1:length(elements)) {
    
    #Create template with all classes for storing confusion matrix results
    confusion_template <- matrix(0,ncol(ED_matrix),ncol(ED_matrix))
    
    #Store confusion matrix k
    confusion_matrix <- confusion_list[[elements[k]]]
    
    #Store confusion matrix from one model in the template (that has all the ecosystem types)
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
      print(k/length(elements))
    }
  }
  
  #Aggregate the confusion matrices with the mean
  mean_confusion_matrix <- apply(confusion_array, c(1,2), mean)
  
  #Return the aggregated confusion matrix
  return(mean_confusion_matrix)
}

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
classifier_confusion <- readRDS("results/classifier_confusion.rds")

#Import confusion matrices for interpreters
interpreter_confusion <- readRDS("results/interpreter_confusion.rds")

#Import model results
classfier_data <- read.csv("results/results.csv")[,-c(1)]

#Import model results
interpreter_data <- read.csv("results/interpreter_results.csv")[,-c(1)]

#Create a data frame with all combinations of interpreters and hierarchical levels
data_frame_classifier <- interpreter_data[,1:2]

#Create list for storing confusion matrices
aggregated_classifier_list <- list()

for (h in 1:nrow(data_frame_classifier)) {
  
  #Specify the correct conversion key (depending on hierarchical level and map scale)
  if(data_frame_classifier$hierarchicallevel[h] == "gtype1") {
    conversion_scheme <- conversion_list[[1]]
    if(data_frame_classifier$interpreter[h] %in% c("AB","AEN","EAF","EL","RH","TS","mode5")) {
      conversion_scheme <- conversion_list[[2]]
    }
  }
  if(data_frame_classifier$hierarchicallevel[h] == "htype1") {
    conversion_scheme <- conversion_list[[3]]
  }
  if(data_frame_classifier$hierarchicallevel[h] == "htypegr1") {
    conversion_scheme <- conversion_list[[4]]
  }
  
  #Identify the rows with the same hierarchical levels and map scales
  interpreter_rows <- which(classfier_data$interpreter == data_frame_classifier$interpreter[h])
  hierarchicallevel_rows <- which(classfier_data$hierarchicallevel == data_frame_classifier$hierarchicallevel[h])
  
  #Specify the row
  rows <- interpreter_rows[which(interpreter_rows %in% hierarchicallevel_rows)]
  
  #Aggregate confusion matrices for classifiers
  aggregated_classifier_list[[h]] <- confusion.matrix(conversion_scheme, classifier_confusion, rows, progress = F)
}

#Create data frame with all combinations of interpreters and hierarchical levels
data_frame_interpreter <- interpreter_data[,1:2]

#Create list for storing confusion matrices
aggregated_interpreter_list <- list()

for (h in 1:nrow(data_frame_interpreter)) {
  
  #Specify the correct conversion key (depending on hierarchical level and map scale)
  if(data_frame_interpreter$hierarchicallevel[h] == "gtype1") {
    conversion_scheme <- conversion_list[[1]]
    if(data_frame_interpreter$interpreter[h] %in% c("AB","AEN","EAF","EL","RH","TS","mode5")) {
      conversion_scheme <- conversion_list[[2]]
    }
  }
  if(data_frame_interpreter$hierarchicallevel[h] == "htype1") {
    conversion_scheme <- conversion_list[[3]]
  }
  if(data_frame_interpreter$hierarchicallevel[h] == "htypegr1") {
    conversion_scheme <- conversion_list[[4]]
  }
  
  #Specify row
  rows <- h
  
  #Aggregate confusion matrices for classifiers
  aggregated_interpreter_list[[h]] <- confusion.matrix(conversion_scheme, interpreter_confusion, rows, progress = F)
}

#Create column for correlation estimates for each interpreter hierarchical level combination
data_frame_classifier$confusion_correlation <- NA

#Loop over all lists with aggregated confusion matrices
for (i in 1:length(aggregated_interpreter_list)) {
  
  #Convert the interpreter confusion matrix for to a vector
  interpreter_confusion_vector <- as.numeric(aggregated_interpreter_list[[i]])
  
  #Convert the classifier confusion matrix for to a vector
  classifier_confusion_vector <- as.numeric(aggregated_classifier_list[[i]])
  
  #Identify confusion matrix cells in which there has been assigned ecosystem types 
  existent_values <- unique(c(which(interpreter_confusion_vector != 0), which(classifier_confusion_vector != 0)))
  
  #Store the correlation between classifiers and interpreter in the data frame
  data_frame_classifier$confusion_correlation[i] <- cor.test(interpreter_confusion_vector[existent_values], classifier_confusion_vector[existent_values], alternative = "two.sided", method = "pearson")$estimate
}

#Print the results
mean(data_frame_classifier$confusion_correlation)
sd(data_frame_classifier$confusion_correlation)

