#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import interpreter data
interpreter_data <- read.csv("results/interpreter_results.csv")[,-c(1)]

#Import classifier data
classifier_data <- read.csv("results/classifier_results.csv")[,-c(1)]

#Create column for interpreter error
classifier_data$interpreter_error <- NA

#Create column for interpreter ecological distance
classifier_data$interpreter_ED <- NA

#Create column for number of classes
classifier_data$interpreter_classes <- NA

#Add interpreter data to classifier data
for (i in 1:nrow(classifier_data)) {
  
  #Specify interpreter
  interpreter <- as.character(classifier_data$interpreter[i])
  
  #Specify hierarchical level
  hierarchical_level <- as.character(classifier_data$hierarchicallevel[i])
  
  #Specify row
  row <- which(interpreter_data$interpreter == interpreter)[which(interpreter_data$interpreter == interpreter) %in% which(interpreter_data$hierarchicallevel == hierarchical_level)]
  
  #Add interpreter error to data frame
  classifier_data$interpreter_error[i] <- interpreter_data$interpreter_error[row]
  
  #Add interpreter ecological distance to data frame
  classifier_data$interpreter_ED[i] <- interpreter_data$interpreter_ED[row]
  
  #Add interpreter ecological distance to data frame
  classifier_data$interpreter_classes[i] <- interpreter_data$interpreter_classes[row]
}

#Calculate difference in error
classifier_data$error_difference <- classifier_data$classifier_error-classifier_data$interpreter_error

#Calculate difference in ecological distance
classifier_data$ED_difference <- classifier_data$classifier_ED-classifier_data$interpreter_ED

#Save data
write.csv(classifier_data, "results/results.csv")