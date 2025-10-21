#Import library
library(ggplot2)
library(grid)
library(scales)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/error_propagation_paper/R/")

#Define function to fit a subset of the data
fit.model <- function(data, factor_level) {
  
  #Subset data
  data <- data[which(data$`Hierarchical level` == factor_level),]
  
  #Fit the linear model using the formula
  model <- lm(`Classifier error` ~ `Interpreter error`, data)
  
  #Create summary object
  summary_model <- summary(model)
  
  #Create a summary
  numeric_vector <- c(summary_model$coefficients[2,1], 
                      summary_model$coefficients[2,2], 
                      summary_model$coefficients[2,4], 
                      summary_model$r.squared)
  
  #Return summary object
  return(numeric_vector)
  
}

#Import data
data <- read.csv("results_ijrs/results.csv")[,-c(1)]

#Rename and reorder map scale levels
data$mapscale <- factor(data$mapscale, 
                        levels = c("5", "20"), 
                        labels = c("1:5000", "1:20 000"))

#Create variable for scale and level
data$thematiclevel <- NA
data$thematiclevel[(data$hierarchicallevel == "htypegr1")] <- "Major-type group"
data$thematiclevel[(data$hierarchicallevel != "htypegr1")] <- "Major type/mapping unit"
data$thematiclevel <- factor(data$thematiclevel, levels = c("Major type/mapping unit","Major-type group"))

#Rename and reorder hierarchical levels
data$hierarchicallevel <- factor(data$hierarchicallevel, 
                                 levels = c("gtype1","htype1","htypegr1"), 
                                 labels = c("Mapping unit","Major type","Major-type group"))

#Rename and reorder interpreter levels
data$interpreter <- factor(data$interpreter, 
                           levels = c("X5","A5","B5","C5","D5","E5","F5","X20","G20","H20","I20","J20","K20","L20"), 
                           labels = c("X5","A5","B5","C5","D5","E5","F5","X20","G20","H20","I20","J20","K20","L20"))

#Create variable for scale and level
data$scaleandlevel <- paste(data$hierarchicallevel,data$mapscale, sep = "_")
data$scaleandlevel <- factor(data$scaleandlevel,
                             levels = c("Mapping unit_1:5000", "Mapping unit_1:20 000", "Major type_1:5000", "Major type_1:20 000", "Major-type group_1:5000", "Major-type group_1:20 000"),
                             labels = c("Mapping unit - 1:5000", "Mapping unit - 1:20 000", "Major type - 1:5000", "Major type - 1:20 000", "Major-type group - 1:5000", "Major-type group - 1:20 000"))

#Convert to factor variables and specify level order
data$resolution <- factor(data$resolution, levels = c("high","medium","low"), labels = c("1.5 \u00D7 1.5", "3 \u00D7 3", "6 \u00D7 6"))
data$nodesize <- factor(data$nodesize, levels = c("0.5","0.25","0.1"), labels = c("50","25","1"))
data$samplesize <- factor(data$samplesize, levels = c("35","350","3500"), labels = c("1","10","100"))
data$method <- factor(data$method, levels = c("rf","svm","knn","nb"), labels = c("RF","SVM","k-NN","Naive Bayes"))
data$outlierfiltering <- factor(data$outlierfiltering, levels = c("0","1"), labels = c("No","Yes"))

#Rename columns
colnames(data) <- c("Method","Node size","Outlier filtering", "Interpreter", "Hierarchical level", "Spatial resolution", "Sampling density", "Map scale", "Thematic detail", 
                    "Experience level", "Out-of-bag", "Classifier error", "Classifier ED", "Classifier classes", "Sample size", "Outliers", "Training distance", "Validation distance", 
                    "Computation time", "Interpreter error", "Interpreter ED", "Interpreter classes", "Error propagation", "ED propagation", "Thematic resolution", "Hierarchical level - map scale")


#Fit the linear model using the formula
model <- lm(`Classifier error` ~ `Hierarchical level` * `Interpreter error`, data)

#Create a model summary
summary_model <- summary(model)

#Set hierarchical levels
hierarchical_levels <- levels(data$`Hierarchical level`)

#Create template matrix
summary_df <- as.data.frame(matrix(data = NA, nrow = length(hierarchical_levels), ncol = 4))

#Loop over all hierarchical levels
for (i in 1:length(hierarchical_levels)) {
  
  #Insert values
  summary_df[i,] <- fit.model(data, hierarchical_levels[i])
}

#Rename columns
colnames(summary_df) <- c("estimate","se","p","r")

#Rename rows
rownames(summary_df) <- hierarchical_levels

#Create the plot
error_plot <- ggplot(data = data, aes(x = `Interpreter error`, y = `Classifier error`)) +
  
  #Add points
  geom_jitter(aes(fill = `Hierarchical level`), 
              alpha = 1, shape = 21, width = 0.01, size = 1.5, color = "black") +
  
  #Add regression lines
  stat_smooth(aes(group = `Hierarchical level`), 
              method = "lm", se = TRUE, size = 1.25, color = "grey", alpha = 0.4, fill = "lightgrey", show.legend = FALSE) +
  
  #Add regression lines
  stat_smooth(aes(color = `Hierarchical level`, fill = `Hierarchical level`), 
              method = "lm", se = TRUE, size = 1.0, alpha = 0.3, show.legend = FALSE) +
  
  #Add diagonal dashed line where x = y
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1.0, alpha = 0.5) +
  
  #Add axis labels
  xlab("Interpreter error") +
  ylab("Classifier error") +
  
  #Set the fill color for points
  scale_fill_manual(name = expression(bold(paste("Hierarchical level (estimate ", "\u00B1", " ", bolditalic(SE), ", ", bolditalic(p), ", ", bolditalic(R)^2, ")"))),
                    values = c("Mapping unit" = "olivedrab1", 
                               "Major type" = "deepskyblue4", 
                               "Major-type group" = "cyan"),
                    labels = c(
                      expression(paste("Type unit (0.55 ", "\u00B1", " 0.02, ", italic(p), " < 0.001, ", italic(R)^2, " = 0.22)")),
                      expression(paste("Major type (0.58 ", "\u00B1", " 0.02, ", italic(p), " < 0.001, ", italic(R)^2, " = 0.26)")),
                      expression(paste("Major-type group (0.16 ", "\u00B1", " 0.03, ", italic(p), " < 0.001, ", italic(R)^2, " = 0.01)"))
                    )) +
  
  #Set the color for regression lines to match point colors
  scale_color_manual(name = expression(bold(paste("Hierarchical level (estimate ", "\u00B1", " ", bolditalic(SE), ", ", bolditalic(p), ", ", bolditalic(R)^2, ")"))),
                     values = c("Mapping unit" = "olivedrab1", 
                                "Major type" = "deepskyblue4", 
                                "Major-type group" = "cyan"),
                     labels = c(
                       expression(paste("Type unit (0.55 ", "\u00B1", " 0.02, ", italic(p), " < 0.001, ", italic(R)^2, " = 0.22)")),
                       expression(paste("Major type (0.58 ", "\u00B1", " 0.02, ", italic(p), " < 0.001, ", italic(R)^2, " = 0.26)")),
                       expression(paste("Major-type group (0.16 ", "\u00B1", " 0.03, ", italic(p), " < 0.001, ", italic(R)^2, " = 0.01)"))
                     )) +
  
  #Set coordinate limits
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  
  #Theme
  theme_bw() +
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 12),
        legend.text.align = 0,  # Left-align legend text
        legend.key.width = unit(0.3, "cm"),  # Reduce space between points and text
        legend.spacing.x = unit(0.1, "cm"),  # Move text closer to legend symbols
        legend.background = element_rect(fill = alpha("white", 0.6)),  # Semi-transparent white background
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black", angle = 90, hjust = 0.55),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)

#Print the plot
print(error_plot)



#Set the dimensions of the plot
plot_width <- 14
plot_height <- plot_width * 0.70

#Save the plot
ggsave("results_ijrs/classifier_interpreter.png", plot = error_plot, width = plot_width, height = plot_height, dpi = 500)
