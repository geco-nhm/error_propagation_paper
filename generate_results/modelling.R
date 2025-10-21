#Import libraries
library(readxl)
library(raster)
library(terra)
library(randomForest)
library(caret)
library(vegan)
library(dplyr)
library(e1071)
library(MASS)
library(robustbase)
library(ellipse)
library(ggplot2)
library(glmnet)
library(nnet)


#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R")

#Set random seed
set.seed(123)

scale.parameter <- function(complexity_value, min_parameter, max_parameter) {
  
  scaled_parameter <- ((max_parameter - min_parameter) * complexity_value) + min_parameter
  
  return(scaled_parameter)
}

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
filter.outliers <- function(data, interpreter_names, feature_names, resolution, hierarchical_level, minimum_observations, proportion, level, generate_plot, robust_centre, robust_cov) {
  
  #Identify the classes
  classes <- unique(data[,interpreter_names])
  
  #Create vector for storing significant p-values
  outlier_distance <- numeric()
  
  #Create vector for storing significant rows
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
        
        #Select title
        class_title <- colnames(conversion_list[[which(hierarchical_levels %in% hierarchical_level)]])[as.numeric(as.character(classes[i]))]
        
        #Plot PCA with outliers
        print(ggplot(pca_scores , aes(x = PC1 , y = PC2, col = outliers)) +
                geom_point(size = 3) +
                geom_polygon(data = ellipse , fill = "orange" , color = "orange" , alpha = 0.2)+
                geom_point(aes(centroid[1] , centroid[2]) , size = 8 , color = "deepskyblue2") +
                ylab("PC1") + xlab("PC2") +
                scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
                scale_y_continuous(labels = function(y) sprintf("%.1f", y)) +
                scale_color_manual(values = c("TRUE" = "red2", "FALSE" = "black")) +
                ggtitle(class_title) +
                theme_bw() +
                guides(col = "none") +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      aspect.ratio = 1,
                      axis.text = element_text(color = "black", size = 10),
                      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
                      axis.title = element_text(size = 16, face = "bold")))
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

#Define function for mapping and saving predictions
map.predictions <- function (model, raster_data, spatial_resolution, plot_map, save_tif, id) {
  
  #Specify the current spatial resolution
  current_resolution <- which(levels(resolution) == resolution)
  
  #Specify raster data
  current_raster <- raster_list[[current_resolution]]
  
  #Ensure that names comply
  names(current_raster) <- paste(names(current_raster), resolution, sep = "_")
  
  #Store the raster data in a data frame
  new_data <- as.data.frame(current_raster)
  
  #Generate predictions with the raster data
  new_predictions <- predict(model, new_data)
  
  #Create a new raster to store the predictions
  new_raster <- current_raster[[1]]
  
  #Assign zero to all the grid cells
  new_raster[which(!is.na(new_raster[]))] <- 0
  
  #Assign the predictions to the raster
  new_raster[] <- as.numeric(as.character(new_predictions))
  
  if(plot_map == TRUE) {
    plot(new_raster)
  }
  
  if(save_tif == TRUE) {
    writeRaster(new_raster, paste("predictions/prediction_",id,".tif", sep = ""))
  }
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
  conversion_list[[i]] <- as.data.frame(readxl::read_xlsx(file_paths[i]))
}

#Save data frame
validation_distance <- read.csv("data/processed/test/test_data_distance.csv")

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

#Import and assemble training data sets
for (i in 1:length(ensemble_list)) {
  
  #Import training data
  training_list[[i]] <- read.csv(paste("data/processed/training/training_data2_",paste(ensemble_list[[i]],collapse = ""),".csv", sep = ""))[-c(1)]
  
  #Remove feature (sol_ndsm)
  training_list[[i]] <- training_list[[i]][,!grepl("sol_ndsm", colnames(training_list[[i]]))]
  
  #Specify column names
  training_labels <- colnames(training_list[[i]])[1:42]
  
  #Convert training labels from numeric to factor
  for (j in training_labels) {
    training_list[[i]][,j] <- as.factor(training_list[[i]][,j])
  }
}

#Specify the maximum sample size
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
  sample_list[[j]] <- sample(training_list[[j]]$ID_low, samplesizes[i], replace = F)
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

sample_training <- sample_training[[7]]
training_list <- training_list[[7]]

#Specify interpreters
interpreters <- c("A5","F5","D5","C5","B5","E5","X5","I20","K20","H20","G20","L20","J20","X20")

#Specify feature names
features <- sub("_low","",colnames(training_list)[grepl("low",colnames(training_list))])[-c(1)]

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
complexity <- c(0.001,0.5,0.999)

#Specify ensembles
#ensembles <- 1:7

#Specify methods
methods <- c("rf","svm","knn")

#Specify outlier filtering
outlierfiltering <- c(0,1)

#Create matrix for storing results
output_matrix <- expand.grid(method = methods,
                             nodesize = complexity,
                             outlierfiltering = outlierfiltering,
                             interpreter = interpreters, 
                             hierarchicallevel = hierarchicallevels,
                             resolution = resolutions,
                             #ntree = ntrees, 
                             #mtry = mtrys, 
                             samplesize = samplesizes)

#Create column for map scale
output_matrix$mapscale <- ifelse(output_matrix$interpreter %in% c("A5","F5","D5","C5","B5","E5","X5"), 5, 20)

#Create column for map scale
output_matrix$thematicresolution <- NA

#Create column for experience level
output_matrix$experiencelevel <- NA

#Specify experience levels for the interpreters
output_matrix$experiencelevel[output_matrix$interpreter %in% c("F5", "K20", "L20", "E5")] <- 1
output_matrix$experiencelevel[output_matrix$interpreter %in% c("D5", "C5", "I20", "J20")] <- 2
output_matrix$experiencelevel[output_matrix$interpreter %in% c("A5", "B5", "H20", "G20")] <- 3
output_matrix$experiencelevel[output_matrix$interpreter %in% c("X5", "X20")] <- 4

#Create column for out of bag error
output_matrix$OOB <- NA

#Create column for test error
output_matrix$classifier_error <- NA

#Create column for test ecological distance
output_matrix$classifier_ED <- NA

#Create column for number of classes
output_matrix$classifier_classes <- NA

#Create column for sample size
output_matrix$sample <- NA

#Create column for presence of outliers
output_matrix$outliers <- NA

#Create column for distance
output_matrix$training_distance <- NA

#Create column for distance
output_matrix$validation_distance <- NA

#Create column for computing time
output_matrix$model_time <- NA

#Create column for gini index
output_matrix$gini_index <- NA

#Create matrix for storing importance metrics
importance_matrix <- as.data.frame(matrix(NA, nrow(output_matrix), number_features))

#Rename columns
colnames(importance_matrix) <- features

#Create data frame for storing spatial error results
spatial_error <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create data frame for storing validation distances
spatial_distance <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create data frame for storing spatial ecological distance results
spatial_ED <- as.data.frame(matrix(NA, nrow(output_matrix), nrow(test_data)))

#Create list for storing confusion matrices
confusion_list <- list()

#Create list for storing filtered observations
filtered_ID_list <- list()

#Import results
#output_matrix <- read.csv("results_ijrs/classifier_results2.csv")[,-c(1)]
#importance_matrix <- read.csv("results_ijrs/feature_importance2.csv")[,-c(1)]
#spatial_error <- read.csv("results_ijrs/spatial_classifier_error2.csv")[,-c(1)]
#spatial_distance <- read.csv("results_ijrs/spatial_distance2.csv")[,-c(1)]
#spatial_ED <- read.csv("results_ijrs/spatial_classifier_ED2.csv")[,-c(1)]

#Import models
#confusion_list <- readRDS("results_ijrs/classifier_confusion2.rds")

#Import list of filtered observations
#filtered_ID_list <- readRDS("results_ijrs/filtered_observations2.rds")

#Create list for raster stacks
raster_list <- list()

#Import raster stacks
for (i in 1:length(resolutions)) {
  
  #Specify file paths
  file_paths <- list.files(paste("data/processed/features/",resolutions[i],sep = ""), pattern = "tif$", full.names = TRUE)
  
  #Store raster stacks in lists
  raster_list[[i]] <- stack(file_paths)
}

#Start time
start_time <- Sys.time()

#Specify start row
start_row <- 1

#Specify end row
end_row <- nrow(output_matrix)

#Generate models and store results
for (i in start_row:end_row) {
  
  #Create training data set
  train_data_loop <- training_list

  #Specify interpreter and map scale
  interpreter <- paste(output_matrix$hierarchicallevel[i],
                       "_",
                       ifelse(grepl(output_matrix$hierarchicallevel[i], "gtype1"), ifelse(output_matrix$interpreter[i] %in% c("A5","F5","D5","C5","B5","E5","X5"),"5_","20_"),""),
                       output_matrix$interpreter[i], sep = "")
  
  #Specify the resolution
  resolution <- output_matrix$resolution[i]
  
  #Specify features
  features_loop <- colnames(train_data_loop)[which(grepl(resolution, colnames(train_data_loop)))][-c(1)]
  
  #Specify test labels
  test_set <- paste(output_matrix$hierarchicallevel[i],
                    ifelse(grepl(output_matrix$hierarchicallevel[i], "gtype1"), ifelse(output_matrix$interpreter[i] %in% c("A5","F5","D5","C5","B5","E5","X5"),"_5","_20"),""), sep = "")
  
  #Store the thematic resolution
  output_matrix$thematicresolution[i] <- test_set
  
  #Specify sample
  train_data_loop <- train_data_loop[which(train_data_loop$ID_low %in% sample_training[[which(samplesizes == output_matrix$samplesize[i])]]),]
  
  #Store the number of data points for each class
  class_table <- table(train_data_loop[,interpreter])
  
  #Exclude classes with fewer than 5 observations if there are more than two classes
  if(length(class_table[which(class_table != 0)]) > 2) {
  
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
                                   test_set,
                                   minimum_observations = 10, 
                                   proportion = 0.25, 
                                   level = 0.95, 
                                   generate_plot = FALSE, 
                                   robust_centre = TRUE, 
                                   robust_cov = TRUE)
    
    #Store the IDs of the filtered observations
    filtered_ID_list[[i]] <- filter_list[[2]]
    
    #Create new data frame without outliers
    train_data_loop <- filter_list[[1]]
    
    #Check if there were any outliers
    output_matrix$outliers[i] <- length(filter_list[[2]])
  }
  
  #Store the mean training distance
  output_matrix$training_distance[i] <- mean(train_data_loop[,paste0(output_matrix$interpreter[i], "_dist")])
  
  #Convert labels to factor
  train_data_loop[,interpreter] <- factor(train_data_loop[,interpreter], levels = as.character(names(which(table(train_data_loop[,interpreter]) != 0))))
  
  #Start the time
  start_model <- Sys.time()
  
  #Random forest
  if(output_matrix$method[i] == "rf") {
    
    #Set scale parameter
    node_size <- scale.parameter(complexity_value = 1 - output_matrix$nodesize[i],
                                 min_parameter = 1,
                                 max_parameter = sqrt(nrow(train_data_loop)))
    
    #Train RF
    #model <- randomForest(formula = as.formula(paste(interpreter,"~",paste(features_loop,collapse = "+"))),
    #                     data = train_data_loop,
    #                      nodesize = node_size,
    #                      #ntree = output_matrix$ntree[i],
    #                      ntree = 500,
    #                      #mtry = output_matrix$mtry[i]
    #                      mtry = sqrt(number_features))
    
    #Support vector machine
  } else if (output_matrix$method[i] == "svm") {
    
    #Set scale parameter
    cost_svm <- scale.parameter(complexity_value = output_matrix$nodesize[i],
                                min_parameter = 0,
                                max_parameter = sqrt(nrow(train_data_loop)))
    
    #Train SVM
    #model <- svm(
    #  formula = as.formula(paste(interpreter, "~", paste(features_loop, collapse = "+"))),
    #  data = train_data_loop,
    #  kernel = "radial",
    #  cost = cost_svm)
    
    #k-nearest neighbour
  } else if (output_matrix$method[i] == "knn") {
    
    #Set scale parameter
    k_nn <- scale.parameter(complexity_value = 1 - output_matrix$nodesize[i],
                            min_parameter = 1,
                            max_parameter = sqrt(nrow(train_data_loop)))
    k_nn <- max(1, round(k_nn))
    if (k_nn %% 2 == 0) {k_nn <- k_nn + 1}  #Make it odd to avoid ties
    
    #Recreate the target data column
    #train_data_loop$target <- factor(train_data_loop[[interpreter]],
    #                                levels = unique(train_data_loop[[interpreter]]))

    #Rebuild the training data frame
    #train_data_loop_clean <- train_data_loop[, c("target", features_loop)]
    
    #Train knn
    #model <- train(
    #  target ~ .,
    #  data = train_data_loop_clean,
    #  method = "knn",
    #  tuneGrid = expand.grid(k = k_nn),
    #  trControl = trainControl(method = "none"))
    
    
  }
  
  #Generate maps
  #map.predictions(model = model,             
  #                raster_data = raster_list, 
  #                spatial_resolution = resolution, 
  #                plot_map = FALSE, 
  #                save_tif = FALSE, 
  #                id = i)
  
  #Calculate OOB error and store feature importance in a data frame
  #if (output_matrix$method[i] == "rf") {
  #  output_matrix$OOB[i] <- model$err.rate[500, 1]
  #  importance_matrix[i,] <- as.numeric(model$importance)
  #} else {
  #  output_matrix$OOB[i] <- NA
  #  importance_matrix[i, ] <- NA
  #}
  
  #Generate predictions on the test data
  #test_predictions <- predict(model, test_data[,features_loop])
  
  #Harmonize the classes in the test data and model predictions and opposite
  #test_predictions <- factor(test_predictions, unique(c(levels(test_data[,test_set]), levels(test_predictions))))
  #test_data[,test_set] <- factor(test_data[,test_set], unique(c(levels(test_data[,test_set]), levels(test_predictions))))
  
  #Compute error
  #output_matrix$classifier_error[i] <- 1-(sum(test_data[,test_set] == test_predictions)/length(test_data[,test_set]))
  
  #Generate the confusion matrix
  #test_confusion <- confusionMatrix(test_predictions, test_data[,test_set])$table
  
  #Store the confusion matrix in a list
  #confusion_list[[i]] <- test_confusion
  
  #Specify the relevant ecological distance matrix
  #ED_matrix <- conversion_list[[which(hierarchical_levels == test_set)]][as.numeric(colnames(test_confusion)),as.numeric(colnames(test_confusion))]
  
  #Compute ecological distance
  #output_matrix$classifier_ED[i] <- sum(test_confusion*ED_matrix)/sum(test_confusion)
  
  #Compute error for each test point
  #spatial_error[i,] <- as.numeric(test_predictions == test_data[,test_set])
  
  #Insert validation distance
  #spatial_distance[i,] <- validation_distance[, paste0(output_matrix$interpreter[i], "_dist")]
  
  #Compute ecological distance for each test point
  #spatial_ED[i,] <- sapply(1:length(test_predictions), compute.ED)
  
  #Store sample size
  #output_matrix$sample[i] <- nrow(train_data_loop)
  
  #Store the number of classes
  #output_matrix$classifier_classes[i] <- length(unique(train_data_loop[,interpreter]))
  
  #Stop time
  end_model <- Sys.time()
  
  #Calculate the duration
  output_matrix$model_time[i] <- end_model-start_model
  
  #Print progress
  print(i/nrow(output_matrix))
  
  #Class distribution
  class_counts <- table(train_data_loop[,interpreter])
  
  #Calculate total number of samples
  total_samples <- sum(class_counts)
  
  #Calculate class proportions
  class_proportions <- class_counts / total_samples
  
  #Calculate gini index
  gini_index <- 1 - sum(class_proportions^2)
  
  #Store gini index
  output_matrix$gini_index[i] <- gini_index
  
  
  #Save files for every 2500th iteration
  if(i %% 1000 == 0) {
    
  #Save results
  #write.csv(output_matrix, "results_ijrs/classifier_results2.csv")
  #write.csv(importance_matrix, "results_ijrs/feature_importance2.csv")
  #write.csv(spatial_error, "results_ijrs/spatial_classifier_error2.csv")
  #write.csv(spatial_distance, "results_ijrs/spatial_distance2.csv")
  #write.csv(spatial_ED, "results_ijrs/spatial_classifier_ED2.csv")
  
  #Save models
  #saveRDS(confusion_list, "results_ijrs/classifier_confusion2.rds")
  
  #Save list of filtered observations
  #saveRDS(filtered_ID_list, "results_ijrs/filtered_observations2.rds")
  }
}

#Stop time
end_time <- Sys.time()

#Calculate the duration
end_time-start_time


#Save results
write.csv(output_matrix, "results_ijrs/classifier_results_gini.csv")