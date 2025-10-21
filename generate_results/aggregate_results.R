#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import interpreter data
interpreter_data <- read.csv("results_ijrs/interpreter_results.csv")[,-c(1)]

#Import classifier data
classifier_data <- read.csv("results_ijrs/classifier_results2.csv")[,-c(1)]

#Reclassify interpreter names
interpreter_data$interpreter <- factor(
  interpreter_data$interpreter,
  levels = unique(interpreter_data$interpreter),
  labels = c("A5", "F5", "D5", "C5", "B5", "E5", "X5", "I20", "K20", "H20", "G20", "L20", "J20", "X20")
)

#Reclassify interpreter names
classifier_data$interpreter <- factor(
  classifier_data$interpreter,
  levels = unique(classifier_data$interpreter),
  labels = c("A5", "F5", "D5", "C5", "B5", "E5", "X5", "I20", "K20", "H20", "G20", "L20", "J20", "X20")
)

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
  
  #Identify current interpreter data
  current_interpreter <- which(interpreter_data$interpreter == interpreter)
  
  #Identify current hierarchical level data
  current_hierarchical_level <- which(interpreter_data$hierarchicallevel == hierarchical_level)
  
  #Specify rows
  rows <- current_interpreter[current_interpreter %in% current_hierarchical_level]
  
  #Add interpreter error to data frame
  classifier_data$interpreter_error[i] <- interpreter_data$interpreter_error[rows]
  
  #Add interpreter ecological distance to data frame
  classifier_data$interpreter_ED[i] <- interpreter_data$interpreter_ED[rows]
  
  #Add interpreter ecological distance to data frame
  classifier_data$interpreter_classes[i] <- interpreter_data$interpreter_classes[rows]
}

#Calculate difference in error
classifier_data$error_difference <- classifier_data$classifier_error-classifier_data$interpreter_error

#Calculate difference in ecological distance
classifier_data$ED_difference <- classifier_data$classifier_ED-classifier_data$interpreter_ED

#Save data
write.csv(classifier_data, "results_ijrs/results.csv")
