#Import libraries
library(readxl)
library(randomForest)
library(caret)
library(vegan)
library(dplyr)
library(e1071)
library(MASS)
library(robustbase)
library(ellipse)
library(ggplot2)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R")

#Set random seed
set.seed(123)

#Define zero skewness transformation function 
standardize <- function(variable){
  scalex <- function(x, c) { 
    if(skewness(x,na.rm=TRUE,type = 2) < 0) 
      return(exp(c*x)) 
    else 
      return(log(x+c))
  }
  
  minskew <- function(x) {
    cmin <- min(x)-10*(max(x)-min(x));
    cmax <- max(x)+10*(max(x)-min(x));
    if(skewness(x,na.rm=TRUE) >= 0 && cmin < -min(x))
      cmin <- -min(x)
    cmid <- (cmin+cmax)/2;
    skew <- skewness(scalex(x, cmid), na.rm=TRUE);
    while (abs(skew) > 1*10^-05 && min(abs(c(cmax, cmin)-cmid)) > 10^-10) {
      sleft <- skewness(scalex(x, (cmid+cmin)/2), na.rm=TRUE)
      sright <- skewness(scalex(x, (cmid+cmax)/2), na.rm=TRUE)
      if (abs(sleft) < abs(skew) && abs(sleft) < abs(sright)) {
        cmax <- cmid
        skew <- sleft
      } else if (abs(sright) < abs(skew)) {
        cmin <- cmid
        skew <- sright
      } else {
        cmin <- (cmin+cmid)/2;
        cmax <- (cmax+cmid)/2;
      }
      cmid <- (cmin + cmax)/2;
    }
    return(list(c = cmid, skew=skew));
  }
  x <- (variable-min(variable))/(max(variable)-min(variable))
  res <- minskew(x)
  standx <- scalex(x,res$c)
  rangx <- (standx-min(standx))/(max(standx)-min(standx))
  return(rangx)
}

#Define function for removing outliers
filter.outliers <- function(data, interpreter_names, feature_names, resolution, minimum_observations, proportion, level, generate_plot, robust_centre, robust_cov) {
  
  #Identify the classes
  classes <- unique(data[,interpreter_names])
  
  #Creating vector for storing significant p-values
  outlier_distance <- numeric()
  
  #Creating vector for storing significant rows
  outlier_rows <- numeric()
  
  for (i in 1:length(classes)) {
    
    #Specify the rows belonging to the class
    rows <- which(data[,interpreter_names] %in% classes[i])
    
    if(length(rows) > minimum_observations) {
      
      #Create data frame containing the relevant classes and features
      data_frame <- data[rows,feature_names]
      
      #Apply zero skewness transformation
      data_frame <- as.data.frame(do.call(cbind, lapply(select_if(data_frame, is.numeric), standardize)))
      
      #Perform principal component analysis
      pca <- summary(rda(data_frame))
      
      #Store the PCA scores in a new data frame
      pca_scores <- as.data.frame(pca$sites[,c(1,2)])
      
      #Calculate robust mean and covariance
      robust_covariance <- cov.mcd(pca_scores)
      
      #Store the centroid coordinates in a new vector
      centroid <- robust_covariance$center
      
      #Store the covariances in a new vector
      covariance <- cov(pca_scores)
      
      if(robust_centre == TRUE) {
        #Robust centre values
        centroid <- colMedians(as.matrix(pca_scores))
      }
      
      if(robust_cov == TRUE) {
        #Store the covariances in a new vector
        covariance <- robust_covariance$cov
      }
      
      #Specify degrees of freedom
      df <- ncol(pca_scores)
      
      #Quantify a confidence interval based on a chi-squared distribution
      threshold <- qchisq(level, df)
      
      #Calculate the Mahalanobis distance for each point
      mahalanobis_distance <- mahalanobis(pca_scores, center = centroid, cov = covariance)
      
      #Identify points outside the confidence interval
      outliers <- mahalanobis_distance > threshold
      
      if(generate_plot == TRUE) {
        
        #Create an ellipse based on the PCA scores
        ellipse <- as.data.frame(ellipse(x = covariance, centre = centroid, t = sqrt(threshold)))
        
        #Assign column names
        colnames(ellipse) <- colnames(pca_scores)
        
        #Create column indicating whether an observation will be removed
        pca_scores$outliers <- outliers
        
        #Plot PCA with outliers
        print(ggplot(pca_scores , aes(x = PC1 , y = PC2, col = outliers)) +
                geom_point(size = 2) +
                geom_polygon(data = ellipse , fill = "orange" , color = "orange" , alpha = 0.2)+
                geom_point(aes(centroid[1] , centroid[2]) , size = 5 , color = "red2") +
                ylab("PC1") + xlab("PC2") +
                scale_color_manual(values = c("TRUE" = "deepskyblue4", "FALSE" = "black")) +
                ggtitle(classes[i]) +
                theme_bw() +
                guides(col = "none"))
      }
      
      #Store the distances of the identified objects
      outlier_distance <- c(outlier_distance, mahalanobis_distance[which(outliers)])
      
      #Identify the corresponding rows in the original data frame
      outlier_ID <- data[rows[which(outliers)],paste("ID_",resolution,sep = "")]
      
      #Check the row numbers of the identified objects
      outlier_rows <- c(outlier_rows, outlier_ID)
    }
  }
  
  #Order the p-values for all classes
  priority <- order(outlier_distance)
  
  #Order the row numbers for all classes according to p-value
  rows_priority <- outlier_rows[priority]
  
  #Calculate the maximum number of observations that will be removed
  filter_proportion <- round(nrow(data)*proportion)
  
  #Identify the observations that will be removed
  filter_observations <- rows_priority[1:filter_proportion]
  
  #Remove missing values from the vector
  filter_observations <- filter_observations[!is.na(filter_observations)]
  
  #Reset the data frame
  data_frame <- data
  
  #Remove the selected observations if there are any
  if(length(filter_observations) > 0) {
    
    #Identify the rows that will be removed
    filter_rows <- which(data[,paste("ID_",resolution,sep = "")] %in% filter_observations)
    
    #Remove rows
    data_frame <- data[-filter_rows,]
  }
  
  #Return the data frame
  return(list(data_frame,filter_observations))
}

#Define function for computing ecological distance
compute.ED <- function(x) {
  conversion_list[[which(hierarchical_levels == test_set)]][test_predictions[x],test_data[,test_set][x]]
}

#Specify file paths
file_paths <- list.files("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/ED/", pattern = "xlsx$", full.names = TRUE)

#Create list for class codes
conversion_list <- list()

#Import files with class names and store them in list
for (i in 1:length(file_paths)) {
  conversion_list[[i]] <- as.data.frame(read_xlsx(file_paths[i]))
}

#Import test data
test_data <- read.csv("data/processed/test/test_data.csv")[-c(1)]

#Remove feature (sol_ndsm)
test_data <- test_data[,!grepl("sol_ndsm", colnames(test_data))]

#Specify column names
test_labels <- colnames(test_data)[1:4]

#Convert test labels from numeric to factor
for (i in test_labels) {
  test_data[,i] <- as.factor(test_data[,i])
}

#Specify number of blocks
number_blocks <- 4

#Create list for overview of ensemble training data set
ensemble_list <- list(c(1),
                      c(2),
                      c(3),
                      c(4),
                      c(1,2),
                      c(3,4),
                      c(1,2,3,4))

#Create list for storing ensemble training data sets
training_list <- list()

#Importing and assembling training data sets
for (i in 1:length(ensemble_list)) {
  
  #Import training data
  training_list[[i]] <- read.csv(paste("data/processed/training/training_data_",paste(ensemble_list[[i]],collapse = ""),".csv", sep = ""))[-c(1)]
  
  #Remove feature (sol_ndsm)
  training_list[[i]] <- training_list[[i]][,!grepl("sol_ndsm", colnames(training_list[[i]]))]
  
  #Specify column names
  training_labels <- colnames(training_list[[i]])[1:42]
  
  #Convert training labels from numeric to factor
  for (j in training_labels) {
    training_list[[i]][,j] <- as.factor(training_list[[i]][,j])
  }
}

# Specify the maximum sample size
max_sample <- nrow(training_list[[1]])

#Specify sample sizes
samplesizes <- c(max_sample/100,max_sample/10,max_sample)

#Create list for storing sample selection for ensembles
sample_training <- list(list(),
                        list(),
                        list(),
                        list(),
                        list(),
                        list(),
                        list())

#Create list for storing sample selection for blocks
sample_list <- list()

#Select samples
for (i in 1:length(samplesizes)) {
for (j in 1:number_blocks) {
  
  #Sample IDs randomly from within each training data set
  sample_list[[j]] <- sample(training_list[[j]]$ID_high, samplesizes[i], replace = F)
}
for (j in 1:length(ensemble_list)) {
  
  #Create vector for aggregating data blocks
  sample_vector <- numeric()
  
  #Specify ensemble
  loop_ensemble <- ensemble_list[[j]]
  for (k in 1:length(loop_ensemble)) {
    
    #Aggregate data blocks
    sample_vector <- c(sample_vector, sample_list[[loop_ensemble[k]]])
  }
  
  #Store sample IDs in a nested list
  sample_training[[j]][[i]] <- sample_vector
}
}

#Specify interpreters
interpreters <- c("AB","AEN","EAF","EL","RH","TS","AKW","CO","ESV","HB","MT","PH","mode5","mode20")

#Specify feature names
features <- sub("_low","",colnames(training_list[[1]])[grepl("low",colnames(training_list[[1]]))])[-c(1)]

#Specify number of features
number_features <- length(features)

#Specify map scales
mapscales <- c("5","20")

#Specify hierarchical levels
hierarchicallevels <- c("gtype1","htype1","htypegr1")

#Specify combined hierarchical level and map scale
hierarchical_levels <- c("gtype1_20","gtype1_5","htype1","htypegr1")

#Specify spatial resolutions
resolutions <- c("low","medium","high")

#Specify node sizes
nodesizes <- c(0.1,0.25,0.5)

#Specify ensembles
ensembles <- 1:7

#Specify outlier filtering
outlierfiltering <- c(0,1)

#Create matrix for storing results
output_matrix <- expand.grid(nodesize = nodesizes,
                             outlierfiltering = outlierfiltering,
                             interpreter = interpreters, 
                             hierarchicallevel = hierarchicallevels,
                             resolution = resolutions,
                             #ntree = ntrees, 
                             #mtry = mtrys, 
                             samplesize = samplesizes,
                             ensemble = ensembles)

#Create column for map scale
output_matrix$mapscale <- ifelse(output_matrix$interpreter %in% c("AB","AEN","EAF","EL","RH","TS","mode5"), 5, 20)

#Create column for map scale
output_matrix$thematicresolution <- NA

#Create column for experience level
output_matrix$experiencelevel <- NA

#Specify experience levels for the interpreters
output_matrix$experiencelevel[output_matrix$interpreter %in% c("AEN", "CO", "MT", "TS")] <- 1
output_matrix$experiencelevel[output_matrix$interpreter %in% c("EAF", "EL", "AKW", "PH")] <- 2
output_matrix$experiencelevel[output_matrix$interpreter %in% c("AB", "RH", "ESV", "HB")] <- 3
output_matrix$experiencelevel[output_matrix$interpreter %in% c("mode5", "mode20")] <- 4

#Create column for out of bag error
output_matrix$OOB <- NA

#Create column for test error
output_matrix$classifier_error <- NA

#Create column for test ecological distance
output_matrix$classifier_ED <- NA

#Create column for sample size
output_matrix$sample <- NA

#Create column for presence of outliers
output_matrix$outliers <- NA

#Create column for computing time
output_matrix$model_time <- NA

#Create matrix for storing importance metrics
importance_matrix <- as.data.frame(matrix(NA, nrow(output_matrix), number_features))

#Rename columns
colnames(importance_matrix) <- features

#Create data frame for storing spatial error results
spatial_error <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create data frame for storing spatial ecological distance results
spatial_ED <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create list for storing confusion matrices
confusion_list <- list()

#Create list for storing filtered observations
filtered_ID_list <- list()

#Save results
#output_matrix <- read.csv("results/classifier_results.csv")[,-c(1)]
#importance_matrix <- read.csv("results/feature_importance.csv")[,-c(1)]
#spatial_error <- read.csv("results/spatial_classifier_error.csv")[,-c(1)]
#spatial_ED <- read.csv("results/spatial_classifier_ED.csv")[,-c(1)]

#Save models
#confusion_list <- readRDS("results/classifier_confusion.rds")

#Save list of filtered observations
#filtered_ID_list <- readRDS("results/filtered_observations.rds")

#Start time
start_time <- Sys.time()

#Specify start row
start_row <- i

#Specify end row
end_row <- nrow(output_matrix)

#Generate models and store results
for (i in start_row:end_row) {
  
  #Specify the ensemble
  ensemble <- output_matrix$ensemble[i]
  
  #Create training data set
  train_data_loop <- training_list[[ensemble]]
  
  #Specify interpreter and map scale
  interpreter <- paste(output_matrix$hierarchicallevel[i],
                       "_",
                       ifelse(grepl(output_matrix$hierarchicallevel[i], "gtype1"), ifelse(output_matrix$interpreter[i] %in% c("AB","AEN","EAF","EL","RH","TS","mode5"),"5_","20_"),""),
                       output_matrix$interpreter[i], sep = "")
  
  #Specify the resolution
  resolution <- output_matrix$resolution[i]
  
  #Specify features
  features_loop <- colnames(train_data_loop)[which(grepl(resolution, colnames(train_data_loop)))][-c(1)]
  
  #Specify test labels
  test_set <- paste(output_matrix$hierarchicallevel[i],
                    ifelse(grepl(output_matrix$hierarchicallevel[i], "gtype1"), ifelse(output_matrix$interpreter[i] %in% c("AB","AEN","EAF","EL","RH","TS","mode5"),"_5","_20"),""), sep = "")
  
  #Store the thematic resolution
  output_matrix$thematicresolution[i] <- test_set
  
  #Specify sample
  train_data_loop <- train_data_loop[which(train_data_loop$ID_high %in% sample_training[[ensemble]][[which(samplesizes == output_matrix$samplesize[i])]]),]
  
  #Exclude classes with fewer than 5 observations if there are more than two classes
  if(length(table(train_data_loop[,interpreter])) > 2) {
  
  #Exclude classes with fewer than 5 observations
  train_data_loop <- train_data_loop[which(train_data_loop[,interpreter] %in% as.numeric(names(table(train_data_loop[,interpreter])[which(table(train_data_loop[,interpreter]) >= 5)]))),]
  }
  
  #Filter outliers
  if(output_matrix$outlierfiltering[i] == 1) {
    
    #Remove multivariate outlier observations
    filter_list <- filter.outliers(train_data_loop, 
                                   interpreter, 
                                   features_loop, 
                                   resolution, 
                                   minimum_observations = 10, 
                                   proportion = 0.25, 
                                   level = 0.95, 
                                   generate_plot = F, 
                                   robust_centre = T, 
                                   robust_cov = T)
    
    #Store the IDs of the filtered observations
    filtered_ID_list[[i]] <- filter_list[[2]]
    
    #Create new data frame without outliers
    train_data_loop <- filter_list[[1]]
    
    #Check if there were any outliers
    output_matrix$outliers <- length(filter_list[[2]])
  }
  
  #Convert labels to factor
  train_data_loop[,interpreter] <- factor(train_data_loop[,interpreter], levels = as.character(names(which(table(train_data_loop[,interpreter]) != 0))))
  
  #Start the time
  start_model <- Sys.time()
  
  #Generate random forest classification
  model <- randomForest(formula = as.formula(paste(interpreter,"~",paste(features_loop,collapse = "+"))),
                        data = train_data_loop,
                        nodesize = ceiling(output_matrix$nodesize[i]*nrow(train_data_loop)),
                        #ntree = output_matrix$ntree[i],
                        ntree = 500,
                        #mtry = output_matrix$mtry[i]
                        mtry = sqrt(number_features))
  
  #Calculate OOB error
  output_matrix$OOB[i] <- model$err.rate[500,1]
  
  #Store feature importance in a data frame
  importance_matrix[i,] <- as.numeric(model$importance)
  
  #Generate predictions on the test data
  test_predictions <- predict(model, test_data[,features_loop])
  
  #Harmonize the classes in the test data and model predictions and opposite
  test_predictions <- factor(test_predictions, unique(c(levels(test_data[,test_set]), levels(test_predictions))))
  test_data[,test_set] <- factor(test_data[,test_set], unique(c(levels(test_data[,test_set]), levels(test_predictions))))
  
  #Compute error
  output_matrix$classifier_error[i] <- 1-(sum(test_data[,test_set] == test_predictions)/length(test_data[,test_set]))
  output_matrix$classifier_error[i]
  #Generate the confusion matrix
  test_confusion <- confusionMatrix(test_predictions, test_data[,test_set])$table
  
  #Store the confusion matrix in a list
  confusion_list[[i]] <- test_confusion
  
  #Specify the relevant ecological distance matrix
  ED_matrix <- conversion_list[[which(hierarchical_levels == test_set)]][as.numeric(colnames(test_confusion)),as.numeric(colnames(test_confusion))]
  
  #Compute ecological distance
  output_matrix$classifier_ED[i] <- sum(test_confusion*ED_matrix)/sum(test_confusion)
  
  #Compute error for each test point
  spatial_error[i,] <- as.numeric(test_predictions == test_data[,test_set])
  
  #Compute ecological distance for each test point
  spatial_ED[i,] <- sapply(1:length(test_predictions), compute.ED)
  
  #Store sample size
  output_matrix$sample[i] <- nrow(train_data_loop)
  
  #Stop time
  end_model <- Sys.time()
  
  #Calculate the duration
  output_matrix$model_time[i] <- end_model-start_model
  
  #Print progress
  print(i/nrow(output_matrix))
  
  #Save files for every 2500th iteration
  if(i %% 2000 == 0) {
    
  #Save results
  write.csv(output_matrix, "results/classifier_results.csv")
  write.csv(importance_matrix, "results/feature_importance.csv")
  write.csv(spatial_error, "results/spatial_classifier_error.csv")
  write.csv(spatial_ED, "results/spatial_classifier_ED.csv")
  
  #Save models
  saveRDS(confusion_list, "results/classifier_confusion.rds")
  
  #Save list of filtered observations
  saveRDS(filtered_ID_list, "results/filtered_observations.rds")
  }
}

#Stop time
end_time <- Sys.time()

#Calculate the duration
end_time-start_time



output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(experiencelevel) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(interpreter) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(ensemble) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(samplesize) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(thematicresolution) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(mapscale) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(hierarchicallevel) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(resolution) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(nodesize) %>% summarize(across(c(classifier_error), mean))
output_matrix[which(!is.na(output_matrix$classifier_error)),] %>% group_by(outlierfiltering) %>% summarize(across(c(classifier_error), mean))


as.data.frame(data %>% group_by(nodesize,outlierfiltering) %>% summarize(across(c(classifier_error), min)))
summary(lm(classifier_error~interpreter_error*nodesize,data))
summary(lm(classifier_error~interpreter_error*sample,data))
summary(lm(classifier_error~interpreter_error*resolution,data))
summary(lm(classifier_error~interpreter_error*outlierfiltering,data))
summary(lm(classifier_error~interpreter_error*ensemble,data))
summary(lm(classifier_error~interpreter_error*thematicresolution,data))

summary(lm(classifier_error~interpreter_error*sample,data))

summary(lm(classifier_error~nodesize,data))

