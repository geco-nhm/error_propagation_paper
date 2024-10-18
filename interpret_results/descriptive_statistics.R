#Import libraries
library(ggplot2)
library(flextable)
library(dplyr)
library(tidyr)
library(tibble)
library(officer)
library(proxyC)

#Set random seed
set.seed(123)

#Specify working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Define function for creating descriptive tables
create.descriptive.table <- function(data,factors,metrics,filename) {
  
  #Create list for storing summary statistics
  summary_list <- list()
  
  for (k in 1:length(factors)) {
    
    #Create data frame with means for different groups
    data_mean <- data %>% group_by(!!sym(factors[k])) %>% summarize(across(metrics,mean))
    
    #Create data frame with standard deviations for different groups
    data_sd <- data %>% group_by(!!sym(factors[k])) %>% summarize(across(metrics,sd))
    
    #Create vector with row names
    row_names <- as.data.frame(data_mean)[,c(1)]
    
    #Convert to data frames and round off to two digits
    data_mean <- as.data.frame(data_mean)[,metrics]
    data_mean <- round(data_mean, digits = 2)
    data_sd <- as.data.frame(data_sd)[,metrics]
    data_sd <- round(data_sd, digits = 2)
    
    #Create a matrix to store concatenated matrices
    concatenated_matrix <- matrix("", nrow = nrow(data_mean)+1, ncol = ncol(data_mean))
    
    #Convert from numeric to character and concatenate matrices cell-wise
    for (i in 1:nrow(data_mean)) {
      for (j in 1:ncol(data_mean)) {
        concatenated_matrix[i+1,j] <- paste(as.character(data_mean[i,j]),"\u00B1", as.character(data_sd[i,j]), sep = " ")
      }
    }
    
    #Convert to data frame
    concatenated_matrix <- as.data.frame(concatenated_matrix)
    
    #Rename columns
    colnames(concatenated_matrix) <- colnames(data_mean)
    
    #Rename rows
    rownames(concatenated_matrix) <- c(as.character(factors[k]),as.character(row_names))
    
    #Store matrix in list
    summary_list[[k]] <- concatenated_matrix
  }
  
  #Concatenate the list to a matrix
  concatenated_list <- do.call(rbind, summary_list)
  
  #Create a table
  summary_table <- flextable(concatenated_list %>% rownames_to_column(" "))
  
  #Create a Word document
  document <- read_docx()
  
  #Add the table to a Word document
  document <- document %>%
    body_add_flextable(value = summary_table)
  
  #Save the Word document
  print(document, target = paste("results/",filename,".docx", sep = ""))
  
  #Return the table
  return(summary_table)
}

#Define function for creating test tables
create.test.table <- function(data,factors,metrics,filename) {
  
  #Create list for storing summary statistics
  summary_list <- list()
  for (l in 1:length(factors)) {
    
    #Create matrix for storing test statistics
    concatenated_matrix <- matrix("", nrow = length(levels(data[,factors[l]]))+1, ncol = length(metrics)*2)
    
    for (k in 1:length(metrics)) {
      
      #Create linear regression model
      model <- lm(data[,metrics[k]]-mean(data[,metrics[k]]) ~ factor(data[,factors[l]]) - 1)
      
      #Create model summary
      model_summary <- summary(model)
      
      #Store estimates
      estimates <- model_summary$coefficients[,1]
      estimates <- round(estimates, digits = 2)
      
      #Store SD errors
      sd_errors <- model_summary$coefficients[,2]
      sd_errors <- round(sd_errors, digits = 3)
      
      #Store p-values
      p_values <- model_summary$coefficients[,4]
      p_values <- round(p_values, digits = 3)
      p_values <- ifelse(p_values == 0, "< 0.001", p_values)
      
      #Store R2 value
      adjusted_r <- model_summary$adj.r.squared
      
      #Store R2 values in the matrix
      concatenated_matrix[1,2*k-1]
      
      #Concatenate and store estimates and standard errors
      concatenated_matrix[1:length(levels(data[,factors[l]]))+1,2*k-1] <- paste(as.character(estimates),"\u00B1", as.character(sd_errors), sep = " ")
      
      #Store p-values in the matrix
      concatenated_matrix[1:length(levels(data[,factors[l]]))+1,2*k] <- p_values
    }
    
    #Convert to data frame
    concatenated_matrix <- as.data.frame(concatenated_matrix)
    
    #Rename columns
    colnames(concatenated_matrix) <- c(metrics[1],"   ",metrics[2],"  ")
    
    #Rename rows
    rownames(concatenated_matrix) <- c(as.character(factors[l]),as.character(levels(data[,factors[l]])))
    
    #Store matrix with test statistics in a list
    summary_list[[l]] <- concatenated_matrix
  }
  
  #Concatenate the list to a matrix
  concatenated_list <- do.call(rbind, summary_list)
  
  #Create a table
  summary_table <- flextable(concatenated_list %>% rownames_to_column(" "))
  
  #Create a Word document
  document <- read_docx()
  
  #Add the table to a Word document
  document <- document %>%
    body_add_flextable(value = summary_table)
  
  #Save the Word document
  print(document, target = paste("results/",filename,".docx", sep = ""))
  
  #Return the table
  return(summary_table)
}

#Classifier summary tables ----

#Import data
data <- read.csv("results/results.csv")[,-c(1)]

#Rename and reorder map scale levels
data$mapscale <- factor(data$mapscale, 
                        levels = c("5", "20"), 
                        labels = c("1:5000", "1:20 000"))

#Rename and reorder experience levels
data$experiencelevel <- factor(data$experiencelevel, 
                               levels = c("1","2","3","4"), 
                               labels = c("Beginner","Intermediate","Experienced","Calibrated"))

#Rename and reorder hierarchical levels
data$hierarchicallevel <- factor(data$hierarchicallevel, 
                                 levels = c("gtype1","htype1","htypegr1"), 
                                 labels = c("Minor type","Major type","Major-type group"))

#Rename and reorder interpreter levels
data$interpreter <- factor(data$interpreter, 
                           levels = c("mode5","AB","RH","EL","EAF","TS","AEN","mode20","HB","ESV","AKW","PH","CO","MT"), 
                           labels = c("Cal5","A5","B5","C5","D5","E5","F5","Cal20","G20","H20","I20","J20","K20","L20"))

#Rename and reorder spatial resolution levels
data$resolution <- factor(data$resolution, 
                           levels = c("low","medium","high"), 
                           labels = c("6 m","3 m","1.5 m"))

#Rename and reorder outlier filtering levels
data$outlierfiltering <- factor(data$outlierfiltering, 
                           levels = c("0","1"), 
                           labels = c("No","Yes"))

#Rename and reorder model complexity levels
data$nodesize <- factor(data$nodesize, 
                                levels = c("0.5","0.25","0.1"), 
                                labels = c("50%","25%","10% "))

#Rename and reorder ensemble levels
data$ensemble <- factor(data$ensemble, 
                        levels = c("1","2","3","4","5","6","7"), 
                        labels = c("Block 1","Block 2","Block 3","Block 4","Block 1:2","Block 3:4","Block 1:4"))

#Rename and reorder sample size levels
data$samplesize <- factor(data$samplesize, 
                        levels = c("35","350","3500"), 
                        labels = c("1%","10%","100%"))

#Rename columns
colnames(data) <- c("Model complexity", "Outlier filtering", "Interpreter", "Hierarchical level", "Spatial resolution",
                    "Sampling density", "Block set", "Map scale", "Thematic resolution", "Experience level", "Out-of-bag error",
                    "Classifier error", "Classifier ED", "Sample size", "Outlier size", "Computation time", "Interpreter error",
                    "Interpreter ED", "Interpreter-classifier difference", "Interpreter-classifier ED difference")

#Create table
create.descriptive.table(data,
                         c("Interpreter","Experience level","Map scale","Hierarchical level"),
                         c("Classifier error","Interpreter-classifier difference"),
                         "pre_classifier_descriptive")

#Create table
create.descriptive.table(data,
                         c("Sampling density","Block set","Spatial resolution","Outlier filtering","Model complexity"),
                         c("Classifier error","Interpreter-classifier difference"),
                         "post_classifier_descriptive")

#Create table
create.test.table(data,
                  c("Interpreter","Experience level","Map scale","Hierarchical level"),
                  c("Classifier error","Interpreter-classifier difference"),
                  "pre_classifier_test")


#Create table
create.test.table(data,
                  c("Sampling density","Block set","Spatial resolution","Outlier filtering","Model complexity"),
                  c("Classifier error","Interpreter-classifier difference"),
                  "post_classifier_test")




#Interpreter summary tables ----

#Import data
data <- read.csv("results/interpreter_results.csv")[,-c(1)]

#Create column for experience level
data$experiencelevel <- NA

#Specify experience levels for the interpreters
data$experiencelevel[data$interpreter %in% c("AEN", "CO", "MT", "TS")] <- 1
data$experiencelevel[data$interpreter %in% c("EAF", "EL", "AKW", "PH")] <- 2
data$experiencelevel[data$interpreter %in% c("AB", "RH", "ESV", "HB")] <- 3
data$experiencelevel[data$interpreter %in% c("mode5", "mode20")] <- 4

#Rename and reorder map scale levels
data$mapscale <- factor(data$mapscale, 
                        levels = c("5", "20"), 
                        labels = c("1:5000", "1:20 000"))

#Rename and reorder experience levels
data$experiencelevel <- factor(data$experiencelevel, 
                               levels = c("1","2","3","4"), 
                               labels = c("Beginner","Intermediate","Experienced","Calibrated"))

#Rename and reorder hierarchical levels
data$hierarchicallevel <- factor(data$hierarchicallevel, 
                                 levels = c("gtype1","htype1","htypegr1"), 
                                 labels = c("Minor type","Major type","Major-type group"))

#Rename and reorder interpreter levels
data$interpreter <- factor(data$interpreter, 
                           levels = c("mode5","AB","RH","EL","EAF","TS","AEN","mode20","HB","ESV","AKW","PH","CO","MT"), 
                           labels = c("X5","A5","B5","C5","D5","E5","F5","X20","G20","H20","I20","J20","K20","L20"))

#Rename columns
colnames(data) <- c("Interpreter", "Hierarchical level", "Map scale", "Interpreter error", "Interpreter ED", "Interpreter classes", "Experience level")

#Create table
create.descriptive.table(data,
                         c("Interpreter","Experience level","Map scale","Hierarchical level"),
                         c("Interpreter error","Interpreter ED"),
                         "interpreter_descriptive")

#Create table
create.test.table(data,
                  c("Interpreter","Experience level","Map scale","Hierarchical level"),
                  c("Interpreter error","Interpreter ED"),
                  "interpreter_test")

#Define function for creating descriptive tables
create.descriptive.table <- function(data,factors,metrics,filename) {
  
  #Create data frame with values for different groups
  data_mean <- data %>% group_by(!!sym(factors[1]),!!sym(factors[2])) %>% reframe(across(metrics))
  
  #Pivot data frame
  data_mean <- data_mean %>% pivot_wider(names_from = !!sym(factors[1]), values_from = metrics)
  
  #Create vector with row names
  row_names <- as.data.frame(data_mean)[,c(1)]
  
  #Remove first column
  data_mean <- data_mean[,-c(1)]
  
  #Convert to data frames and round off to two digits
  data_mean <- as.data.frame(data_mean)
  data_mean <- round(data_mean, digits = 2)
  
  #Rename rows
  rownames(data_mean) <- row_names
  
  #Divide data frame
  data_mean_list <- list(data_mean[1:7,], data_mean[8:14,])
  
  
  for (i in 1:length(data_mean_list)) {
    
    #Calculate means
    column_mean <- colMeans(data_mean_list[[i]])
    column_mean <- round(column_mean, digits = 2)
    
    #Calculate standard deviations
    column_sd <- colSds(as.matrix(data_mean_list[[i]]))
    column_sd <- round(column_sd, digits = 2)
    
    #Aggregate means and standard deviations
    summary_statistics <- paste(column_mean,"\u00B1", column_sd, sep = " ")
    
    #Create row name for mean and standard deviation
    summary_string <- paste("Mean","\u00B1","SD")
    
    #Add the row to the data frame
    data_mean_list[[i]] <- rbind(character(3), data_mean_list[[i]], summary_statistics)
    
    #Rename the row
    rownames(data_mean_list[[i]])[nrow(data_mean_list[[i]])] <- summary_string
    
    #Rename the first row
    rownames(data_mean_list[[i]])[1] <- c("1:5000","1:20 000")[i]
  }
  
  concatenated_list <- do.call(rbind,data_mean_list)
  
  #Create a table
  summary_table <- flextable(concatenated_list %>% rownames_to_column("Interpreter"))
  
  #Create a Word document
  document <- read_docx()
  
  #Add the table to a Word document
  document <- document %>%
    body_add_flextable(value = summary_table)
  
  #Save the Word document
  print(document, target = paste("results/",filename,".docx", sep = ""))
  
  #Return the table
  return(summary_table)
}

#Create table
create.descriptive.table(data,
                         c("Hierarchical level","Interpreter"),
                         c("Interpreter error"),
                         "interpreter_descriptive")

#Hypothesis tests
vector_1 <- data$`Interpreter error`[which(data$`Hierarchical level` == "Minor type")]
vector_2 <- data$`Interpreter error`[which(data$`Hierarchical level` == "Major type")]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

vector_1 <- data$`Interpreter error`[which(data$`Hierarchical level` == "Minor type")]
vector_2 <- data$`Interpreter error`[which(data$`Hierarchical level` == "Major-type group")]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

vector_1 <- data$`Interpreter error`[which(data$`Hierarchical level` == "Major type")]
vector_2 <- data$`Interpreter error`[which(data$`Hierarchical level` == "Major-type group")]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

vector_1 <- data$`Interpreter error`[which(data$`Map scale` == "1:20 000")[which(data$`Map scale` == "1:20 000") %in% which(data$`Hierarchical level` == "Minor type")]]
vector_2 <- data$`Interpreter error`[which(data$`Map scale` == "1:5000")[which(data$`Map scale` == "1:5000") %in% which(data$`Hierarchical level` == "Minor type")]]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

vector_1 <- data$`Interpreter error`[which(data$`Map scale` == "1:20 000")[which(data$`Map scale` == "1:20 000") %in% which(data$`Hierarchical level` == "Major type")]]
vector_2 <- data$`Interpreter error`[which(data$`Map scale` == "1:5000")[which(data$`Map scale` == "1:5000") %in% which(data$`Hierarchical level` == "Major type")]]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

vector_1 <- data$`Interpreter error`[which(data$`Map scale` == "1:20 000")[which(data$`Map scale` == "1:20 000") %in% which(data$`Hierarchical level` == "Major-type group")]]
vector_2 <- data$`Interpreter error`[which(data$`Map scale` == "1:5000")[which(data$`Map scale` == "1:5000") %in% which(data$`Hierarchical level` == "Major-type group")]]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

#Hypothesis tests
vector_1 <- data$`Interpreter error`[which(data$`Experience level` == "Beginner")]
vector_2 <- data$`Interpreter error`[which(data$`Experience level` == "Intermediate")]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)

vector_1 <- data$`Interpreter error`[which(data$`Experience level` == "Experienced")]
vector_2 <- data$`Interpreter error`[which(data$`Experience level` == "Intermediate")]
t.test(vector_1, vector_2, alternative = "two.sided", paired = TRUE)
