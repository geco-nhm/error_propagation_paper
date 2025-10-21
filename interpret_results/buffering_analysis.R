#Import libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/error_propagation_paper/R/")

#Import interpreter data
interpreter_data <- read.csv("results_ijrs/interpreter_results.csv")[1:14,-c(1)]

#Import buffer area
proportion_matrix <- read.csv("results_ijrs/proportion_matrix.csv")[,-c(1)]

#Import data
spatial_error <- read.csv("results_ijrs/spatial_interpreter_error.csv")[1:14,-c(1)] %>% as.matrix()
spatial_distance <- read.csv("results_ijrs/spatial_interpreter_distance2.csv")[1:14,-c(1)] %>% as.matrix()

#Create a matrix to store classifier error for different buffers
buffer_matrix <- matrix(data = NA, nrow = nrow(spatial_error), ncol = 31) %>% as.data.frame()
data_matrix <- matrix(data = NA, nrow = nrow(spatial_error), ncol = 31) %>% as.data.frame()
colnames(buffer_matrix) <- 0:30
colnames(data_matrix) <- 0:30

#Compute classifier error for buffers of 1-30 m
for (j in 1:nrow(buffer_matrix)) {
  for (i in 1:ncol(buffer_matrix)) {
    buffer_matrix[j,i] <- 1 - mean(spatial_error[j,spatial_distance[j,] > i])
    data_matrix[j,i] <- sum(spatial_distance[j,] > i)
  }
  print(j/nrow(buffer_matrix))
}

#Compute the mean difference in classifier error for different errors
for (i in 1:ncol(buffer_matrix)) {
  
  print("")
  print(i)
  print(mean(buffer_matrix[interpreter_data$mapscale == 20,i], na.rm = TRUE))
  print(mean(data_matrix[interpreter_data$mapscale == 20,i], na.rm = TRUE))
  print("")
  
}

#Compute the mean difference in classifier error for different errors
for (i in 1:ncol(buffer_matrix)) {
  
  print("")
  print(i)
  print(mean(buffer_matrix[interpreter_data$mapscale == 5,i], na.rm = TRUE))
  print(mean(data_matrix[interpreter_data$mapscale == 5,i], na.rm = TRUE))
  print("")
  
}


#Bind together
buffer_data <- cbind(mapscale = interpreter_data$mapscale, buffer_matrix)

#Bind together
number_data <- cbind(mapscale = interpreter_data$mapscale, proportion_matrix)
colnames(number_data)[2:32] <- 0:30

#Pivot from wide to long
buffer_long <- buffer_data %>%
  pivot_longer(cols = `0`:`30`, names_to = "distance", values_to = "error") %>%
  mutate(distance = as.numeric(distance))

#Pivot from wide to long
number_long <- number_data %>%
  pivot_longer(cols = `0`:`30`, names_to = "distance", values_to = "error") %>%
  mutate(distance = as.numeric(distance))


#Merge counts into the long data
buffer_long$rel_error <- number_long$error

#Normalize N to a usable scale for box width
buffer_long <- buffer_long %>%
  group_by(distance) %>%
  mutate(rel_width = rel_error / max(rel_error, na.rm = TRUE)) %>%
  ungroup()

#Prepare data
df_summary <- buffer_long %>%
  group_by(distance, mapscale) %>%
  summarise(proportion = mean(rel_error), .groups = "drop")

df_summary <- df_summary %>%
  mutate(mapscale = factor(mapscale, levels = c("5","20"))) %>%
  arrange(distance, mapscale)  # mapscale order matters here


#Set colors
color_legend <- c("5" = "deepskyblue4", "20" = "khaki")

dodge_width <- 0.9
bar_width <- 0.75


#Create top plot
p_top <- ggplot(df_summary, aes(x = factor(distance), y = proportion * 100, fill = factor(mapscale))) +
  geom_col(position = position_dodge(width = dodge_width), 
           width = bar_width, 
           color = "black", 
           alpha = 1) +
  scale_fill_manual(
    values = color_legend,
    labels = c("5" = "1:5000", "20" = "1:20000"),
    name = "Map scale"
  ) +
  labs(y = "Area (% of total extent)", x = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(0, 5, 0, 5),
    legend.position = c(0.9, 0.9),
    legend.justification = c(0, 1),
    legend.box.just = "right",
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 12),
    legend.text.align = 0,
    legend.key.width = unit(0.3, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    axis.text.y = element_text(size = 12, color = "black", angle = 90, hjust = 0.55),
    axis.title.y = element_text(size = 15),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


#Create bottom plot
p_bottom <- ggplot(buffer_long, aes(x = factor(distance), y = error, fill = factor(mapscale))) +
  geom_boxplot(position = position_dodge(width = dodge_width), width = bar_width) +
  scale_fill_manual(
    values = color_legend,
    labels = c("5" = "1:5000", "20" = "1:20000"),
    name = "Map scale"
  ) +
  scale_y_continuous(breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70)) +
  labs(x = "Buffer (m)", y = "Interpreter error", fill = "Map scale") +
  theme_bw() +
  guides(fill = "none") + 
  theme(plot.margin = margin(0, 5, 5, 5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black", angle = 90, hjust = 0.55),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

#Combine plots
buffer_plot <- p_top / p_bottom + plot_layout(heights = c(1, 3))



#Print the plot
print(buffer_plot)

#Set the dimensions of the plot
plot_width <- 14
plot_height <- plot_width * 0.70

#Save the plot
ggsave("results_ijrs/buffer_plot.png", plot = buffer_plot, width = plot_width, height = plot_height, dpi = 500)


colMeans(number_data[1:7,])
colMeans(buffer_matrix[1:7,])
