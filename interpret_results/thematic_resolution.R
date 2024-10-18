#Import libraries
library(ggplot2)
library(cowplot)
library(mgcv)

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Import data
data <- read.csv("results/interpreter_results.csv")[,-c(1)]

#Rename and reorder map scale levels
data$mapscale <- factor(data$mapscale, 
                        levels = c("5", "20"), 
                        labels = c("1:5000", "1:20 000"))

#Rename and reorder hierarchical levels
data$hierarchicallevel <- factor(data$hierarchicallevel, 
                                 levels = c("gtype1","htype1","htypegr1"), 
                                 labels = c("Mapping unit","Major type","Major-type group"))

#Rename and reorder interpreter levels
data$interpreter <- factor(data$interpreter, 
                           levels = c("X5","A5","B5","C5","D5","E5","F5","X20","G20","H20","I20","J20","K20","L20"), 
                           labels = c("X5","A5","B5","C5","D5","E5","F5","X20","G20","H20","I20","J20","K20","L20"))

#Create variable for th ecombination of hierarchical level and maps
data$scaleandlevel <- paste(data$hierarchicallevel, data$mapscale, sep = "_")
data$scaleandlevel <- factor(data$scaleandlevel,
                             levels = c("Mapping unit_1:5000", "Mapping unit_1:20 000", "Major type_1:5000", "Major type_1:20 000", "Major-type group_1:5000", "Major-type group_1:20 000"),
                             labels = c("Mapping unit - 1:5000", "Mapping unit - 1:20 000", "Major type - 1:5000", "Major type - 1:20 000", "Major-type group - 1:5000", "Major-type group - 1:20 000"))

#Rename columns
colnames(data) <- c("Interpreter", "Hierarchical level", "Map scale", "Interpreter error", "Interpreter ED",
                    "Number of ecosystem types", "Hierarchical level - Map scale")

#Reorder the factor
data$`Hierarchical level - Map scale` <- factor(data$`Hierarchical level - Map scale`,
                             levels = c("Mapping unit - 1:5000", 
                                        "Major type - 1:5000", 
                                        "Major-type group - 1:5000", 
                                        "Mapping unit - 1:20 000", 
                                        "Major type - 1:20 000", 
                                        "Major-type group - 1:20 000"))

#Create the plot
error_plot <- ggplot(data, aes(x = `Number of ecosystem types`, y = `Interpreter error`)) +
  
  #Add black outline
  stat_smooth(data = subset(data, `Map scale` == "1:5000"), 
              aes(color = `Hierarchical level - Map scale`), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", k = 3), 
              size = 2.5, color = "black") +  # Thicker black line for the outline
  
  #Add GAM
  stat_smooth(data = subset(data, `Map scale` == "1:5000"), 
              aes(color = `Hierarchical level - Map scale`), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", k = 3), 
              size = 1, color = "salmon") +
  
  #Add black outline
  stat_smooth(data = subset(data, `Map scale` == "1:20 000"), 
              aes(color = `scaleandlevel`), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", k = 5), 
              size = 2.5, color = "black") +  # Thicker black line for the outline
  
  #Add GAM
  stat_smooth(data = subset(data, `Map scale` == "1:20 000"), 
              aes(color = `Hierarchical level - Map scale`), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", k = 5), 
              size = 1, color = "lightblue") +
  
  #Add points
  geom_jitter(aes(fill = `Hierarchical level - Map scale`), alpha = 0.8, width = 0.01, shape = 21, size = 5) +
  
  ylab("Interpreter error") +
  xlab("Number of ecosystem types") +
  
  #Set the color palette for the points
  scale_fill_manual(name = "Hierarchical level - map scale",
                    values = c("Mapping unit - 1:5000" = "darkred", 
                               "Major type - 1:5000" = "red", 
                               "Major-type group - 1:5000" = "coral", 
                               "Mapping unit - 1:20 000" = "darkblue", 
                               "Major type - 1:20 000" = "blue", 
                               "Major-type group - 1:20 000" = "lightblue"),
                    labels = c("Mapping unit - 1:5000", "Major type - 1:5000", 
                               "Major-type group - 1:5000", "Mapping unit - 1:20 000", 
                               "Major type - 1:20 000", "Major-type group - 1:20 000")) +
  
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 0.70)) +
  theme_bw() +
  theme(legend.position = c(0.01, 0.99),
        legend.background = element_rect(fill = "transparent"),
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black", angle = 90, hjust = 0.55),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)

#Set the dimensions of the plot
plot_width <- 14
plot_height <- plot_width * 0.70

#Save the plot
ggsave("thematic_resolution2.png", plot = error_plot, width = plot_width, height = plot_height, dpi = 500)
