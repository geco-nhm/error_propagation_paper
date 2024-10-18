#Import libraries
library(ggplot2)
library(gridExtra)

#Set random seed
set.seed(123)

#Specify working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import data
data <- read.csv("results/results.csv")[,-c(1)]

#Convert to factor variables and specify level order
data$resolution <- factor(data$resolution, levels = c("low","medium","high"))
data$nodesize <- factor(data$nodesize, levels = c("0.5","0.25","0.1"))
data$samplesize <- factor(data$samplesize, c("35","350","3500"))
data$ensemble <- factor(data$ensemble, levels = c("1","2","3","4","5","6","7"))
data$outlierfiltering <- factor(data$outlierfiltering, levels = c("0","1"))

#Specify the x-labels
plot_labels <- list(c("1" = "25", "2" = "25", "3" = "25","4" = "25", "5" = "50", "6" = "50", "7" = "100"),
                    c("35" = "1", "350" = "10", "3500" = "100"),
                    c("high" = "1.5 \u00D7 1.5", "medium" = "3 \u00D7 3", "low" = "6 \u00D7 6"),
                    c("No", "Yes"),
                    c("50", "25", "1"))

#Specify names of x-axes
axis_names <- c("Sampling extent (%)", "Sampling density (%)", "Spatial resolution (m)", "Outlier filtering", "Node size (%)")

#Specify factors
factors <- c("ensemble","samplesize","resolution","outlierfiltering","nodesize")

#Create list for storing the plots
plot_list <- list()

#Create plots
for (i in 1:length(factors)) {
  
  #Store interpreter-classifer difference plots in a list
  plot_list[[i]] <- ggplot(data = data, aes_string(x = factors[i], y = "classifier_error")) +
    geom_boxplot(data = data, aes_string(x = factors[i], y = "classifier_error"), fill = "deepskyblue4", alpha = 0.3) +
    scale_y_continuous(name = "Classifier error", breaks = seq(0,1,0.25)) +
    scale_x_discrete(name = axis_names[i], labels = plot_labels[[i]]) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, color = "black"), # Increase x-axis text size
          axis.text.y = element_text(size = 12, color = "black", angle = 90, hjust = 0.55), # Increase y-axis text size
          axis.title.x = element_text(size = 15), # Increase x-axis label text size
          axis.title.y = element_text(size = 15),         # Align legend text to the left
          panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank(), # Align legend text to the left
          aspect.ratio = 1)
}

#Remove title on y-axis for all but the first plot
for(i in c(2,3,5)){
  plot_list[[i]] <- plot_list[[i]] +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())
}

#Arrange plots next to each other
arranged_plots <- ggpubr::ggarrange(plot_list[[1]], ggplot() + theme_void(), plot_list[[2]], ggplot() + theme_void(), plot_list[[3]],
                                    plot_list[[4]], ggplot() + theme_void(), plot_list[[5]],
                                    nrow = 2,
                                    ncol = 5,
                                    widths = c(1, 0.1, 1, 0.1, 1, 1, 0.1, 1),
                                    legend = "none",
                                    align = "hv")

#Specify plot height and width
plot_width <- 14
plot_height <- plot_width*0.70

#Save the plot
ggsave("classifier_error.png", plot = arranged_plots, width = plot_width, height = plot_height, dpi = 500)