#Import libraries
library(readxl)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import data
data <- as.data.frame(read_xlsx("data/raw/test/test_full.xlsx"))

#Identify disagreements
accuracy_list <- lapply(data[,2:5], function(x, y) { x == y }, data$consensus)

#Bind data frames
accuracy_df <- do.call(cbind, accuracy_list)

#Sum over columns
accuracy_vector <- colSums(accuracy_df)/nrow(accuracy_df)

#Compute mean and sd
mean(accuracy_vector)
sd(accuracy_vector)
