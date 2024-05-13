#Import libraries
library(raster)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import data
data <- read.csv("results/results.csv")[,-c(1)]

#Specify resolution
resolution <- c("low","medium","high")

#Create raster layers with number of times each grid cell was filtered
for (i in 1:length(resolution)) {
  
  #Identify elements of the same resolution from the list
  list_elements <- which(data$resolution == resolution[i])
  
  #Import list with filtered observations
  outlier_list <- readRDS("results/filtered_observations.rds")
  
  #Identify the outliers
  outliers <- unlist(lapply(list_elements, function(i){outlier_list[i]}))
  
  #Identify unique outliers
  outlier_table <- table(outliers)
  
  #Create mask
  raster_layer <- raster(paste("data/processed/features/",resolution[i],"/blue_med.tif", sep = ""))
  raster_layer[which(!is.na(raster_layer[]))] <- 0
  
  #Rename the raster
  names(raster_layer) <- resolution[i]
  
  #Assign the number of times an outlier was filtered to each cell
  raster_layer[as.numeric(names(outlier_table))] <- as.numeric(outlier_table)
  
  #Plot the raster
  plot(raster_layer)
  
  #Save file
  writeRaster(raster_layer, paste("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/QGIS/outlier_",resolution[i],".tif", sep = ""), overwrite = T)
}
