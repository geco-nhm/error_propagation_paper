#Import libraries
library(ggplot2)
library(dplyr)
library(lsmeans)
library(cowplot)

#Set random seed
set.seed(123)

#Set working directory
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 4/R/")

#Define function to perform a one-sample t-test
custom.t.test <- function(beta_hat, se_beta_hat, mu_0, df) {
  
  #Calculate the t-statistic
  t_statistic <- (beta_hat - mu_0) / se_beta_hat
  
  #Calculate the p-value for the two-tailed test
  p_value <- 2 * pt(-abs(t_statistic), df)
  
  #Return the results as a list
  return(p_value)
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
data$nodesize <- factor(data$nodesize, levels = c("0.999","0.5","0.001"), labels = c("High","Intermediate","Low"))
data$samplesize <- factor(data$samplesize, levels = c("35","350","3500"), labels = c("1","10","100"))
data$outlierfiltering <- factor(data$outlierfiltering, levels = c("0","1"), labels = c("No","Yes"))

#Rename columns
colnames(data) <- c("Method","Node size","Outlier filtering", "Interpreter", "Hierarchical level", "Spatial resolution", "Sampling density", "Map scale", "Thematic detail", 
                    "Experience level", "Out-of-bag", "Classifier error", "Classifier ED", "Classifier classes", "Sample size", "Outliers", "Traning distance", "Validation distance", 
                    "Computation time", "Interpreter error", "Interpreter ED", "Interpreter classes", "Error propagation", "ED propagation", "Thematic resolution", "Hierarchical level - map scale")

#Specify names of x-axes
legend_names <- c("Sampling density (%)", "Spatial resolution (m)", "Outlier filtering", "Node size (%)")

#Fit the linear model using the formula
model <- lm(`Classifier error` ~ `Hierarchical level` * `Interpreter error`, data)

#Create a model summary
summary_model <- summary(model)
print(summary_model)

#With separation between thematic resolution
res_var <- "`Classifier error`"
exp_var <- c("`Sampling density`","`Spatial resolution`","`Outlier filtering`","`Node size`")
int_var1 <- "`Interpreter error`"
effect_size_list <- list(list(),list(),list())
trend_list <- list(list(),list(),list())
upper_list <- list(list(),list(),list())
lower_list <- list(list(),list(),list())
var_list <- list(list(),list(),list())
group_list <- list(list(),list(),list())
level_list <- list(list(),list(),list())
level_indicator_list <- list(list(),list(),list())
means_est_list <- list(list(),list(),list())
upper_est <- list(list(),list(),list())
lower_est <- list(list(),list(),list())
p_est <- list(list(),list(),list())

means_est <- numeric()
df_est <- numeric()
se_est <- numeric()
lower_ci <- numeric()
upper_ci <- numeric()

for (j in 1:length(levels(data$`Hierarchical level`))) {
  
  data_subset <- data[which(data$`Hierarchical level` == levels(data$`Hierarchical level`)[j]),]
  
  #Construct the formula as a string
  formula_string <- paste(res_var,"~", int_var1)
  
  #Convert the formula string to a formula object
  model_formula <- as.formula(formula_string)
  
  #Fit the linear model using the formula
  model <- lm(model_formula, data_subset)
  
  #Create a model summary
  summary_model <- summary(model)
  print(summary_model)
  
  means_est[j] <- summary_model$coefficients[2,1]
  df_est[j] <- summary_model$df[2]
  se_est[j] <- summary_model$coefficients[2,2]
  lower_ci[j] <- means_est[j] - (1.96*se_est[j])
  upper_ci[j] <- means_est[j] + (1.96*se_est[j])
  
  for (i in 1:length(exp_var)) {
    
    #Replace non-alphanumeric characters with underscores
    current_exp_var <- gsub("`", "", exp_var[i])
    
    #Change data type to factor
    data_subset[[current_exp_var]] <- factor(data_subset[[current_exp_var]])
    
    #Construct the formula as a string
    formula_string <- paste(res_var,"~", exp_var[i], "*",int_var1)
    
    #Convert the formula string to a formula object
    model_formula <- as.formula(formula_string)
    
    #Fit the linear model using the formula
    model <- lm(model_formula, data_subset)
    
    #Create a model summary
    summary_model <- summary(model)
    print(summary_model)
    
    #Fit an ANOVA
    anova_result <- anova(model)
    print(anova_result)
    
    #Construct the formula as a string
    formula_string <- paste("~", exp_var[i])
    
    #Convert the formula string to a formula object
    model_formula <- as.formula(formula_string)
    
    #Compute slopes, standard errors, and confidence intervals
    model_trends <- lstrends(model, model_formula, var = "Interpreter error")
    
    #Compare slopes
    slope_test <- pairs(model_trends)
    print(slope_test)
    
    #Print the slopes
    model_trends <- as.data.frame(model_trends)
    print(model_trends)
    
    trend_list[[j]][[i]] <- model_trends$`Interpreter error.trend`
    upper_list[[j]][[i]] <- model_trends$`upper.CL`
    lower_list[[j]][[i]] <- model_trends$`lower.CL`
    var_list[[j]][[i]] <- rep(current_exp_var,length(levels(model_trends[,current_exp_var])))
    level_list[[j]][[i]] <- levels(model_trends[,current_exp_var])
    group_list[[j]][[i]] <- rep(levels(data$`Hierarchical level`)[j],length(levels(model_trends[,current_exp_var])))
    means_est_list[[j]][[i]] <- rep(means_est[j],length(levels(model_trends[,current_exp_var])))
    upper_est[[j]][[i]] <- rep(upper_ci[j],length(levels(model_trends[,current_exp_var])))
    lower_est[[j]][[i]] <- rep(lower_ci[j],length(levels(model_trends[,current_exp_var])))
    
    #Ensure the factor column is a factor
    model_trends[[current_exp_var]] <- factor(model_trends[[current_exp_var]])
    
    p_values <- numeric()
    for (k in 1:length(model_trends$`Interpreter error.trend`)) {
      
      #Perform the t-test
      p_values[k] <- custom.t.test(model_trends$`Interpreter error.trend`[k], model_trends$SE[k], means_est[j], df_est[j])
    }
    
    p_est[[j]][[i]] <- p_values
  }
}

trend_data <- data.frame("Interpreter.error trend" = unlist(trend_list), 
                         upper.CL = unlist(upper_list), lower.CL = unlist(lower_list), 
                         exp_var = unlist(var_list), level = unlist(level_list), 
                         group = unlist(group_list), level_indicator = rep(1:11,3),
                         means_est = unlist(means_est_list), upper_ci = unlist(upper_est),
                         lower_ci = unlist(lower_est), p_values = unlist(p_est))

#Rename and reorder hierarchical levels
trend_data$exp_var <- factor(trend_data$exp_var, 
                             levels = c("Sampling density","Spatial resolution","Outlier filtering","Node size"))

trend_data$level_indicator <- factor(trend_data$level_indicator)

#Create a new variable that combines level and group to ensure unique labels
trend_data <- trend_data %>% mutate(combined_label = interaction(exp_var, level, sep = " - "))
trend_data$combined_label <- factor(trend_data$combined_label, 
                                    levels = c("Node size - 50", "Node size - 25", "Node size - 1", "Outlier filtering - No", "Outlier filtering - Yes", "Spatial resolution - 6 \u00D7 6", "Spatial resolution - 3 \u00D7 3", "Spatial resolution - 1.5 \u00D7 1.5", "Sampling density - 1", "Sampling density - 10", "Sampling density - 100"))

#Rename and reorder hierarchical levels
trend_data$group <- factor(trend_data$group, 
                           levels = c("Major type/mapping unit","Major-type group"))

#Specify names of x-axes
legend_names <- c("Sampling density (%)", "Spatial resolution (m)", "Outlier filtering", "Node size (%)")
x_axis_names <- c("50", "25", "1", "No", "Yes", "6 \u00D7 6", "3 \u00D7 3", "1.5 \u00D7 1.5", "1", "10", "100")
x_axis_names <- rev(x_axis_names)

trend_data <- trend_data %>%
  mutate(y_adjusted = case_when(
    group == "Major-type group" ~ as.numeric(as.factor(combined_label)) + 0.2,  # Shift up
    group == "Major type/mapping unit" ~ as.numeric(as.factor(combined_label)) - 0.2,  # Shift down
    TRUE ~ as.numeric(as.factor(combined_label))  # No adjustment
  ))

#Create the ggplot
error_propagation <- ggplot(trend_data, aes(x = `Interpreter.error.trend`, y = y_adjusted)) +
  geom_rect(aes(xmin = lower_ci, xmax = upper_ci, ymin = 0, ymax = 11.5), fill = "gray", alpha = 0.3) +
  scale_y_discrete(labels = function(y) gsub(".*-\\s*", "", y)) +
  geom_segment(data = trend_data, aes(x = means_est, xend = means_est, y = 0, yend = 11.5), 
               linetype = "dotted", color = "black", size = 0.5) +
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(xmin = `lower.CL`, xmax = `upper.CL`), width = 0.05, size = 1) +
  theme_minimal() +
  annotate("text", x = means_est, y = 11.95, label = levels(trend_data$group), size = 5, color = "black", fontface = "bold") +
  annotate("text", x = -0.05, y = 11:1, label = x_axis_names, size = 4, color = "black", hjust = 0, vjust = 0.5) +
  annotate("text", x = 0.15, y = -0.5, label = "Estimated effect of interpreter error on classifier error", size = 5, color = "black", hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = ifelse(group == "Major-type group", upper_ci + 0.12, upper_ci + 0.03), 
                label = sprintf("italic(p) == %.3f", p_values)), parse = TRUE, size = 4, hjust = 0, vjust = 0.5)  +
  annotate("text", x = -0.15, y = c(11,8,5,3), label = legend_names, size = 5, color = "black", fontface = "bold", hjust = 0, vjust = 0.5) +
  annotate("text", x = c(0.0,0.1,0.2,0.3), y = -0.25, label = c("0.0","0.1","0.2","0.3"), size = 4, color = "black", hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, 11.5), xlim = c(-0.30,1.0)) + 
  theme(strip.text = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "cm"))

error_propagation

plot_width <- 12
plot_height <- 17

#Save the plot
ggsave("error_propagation6.png", plot = error_propagation, width = plot_width, height = plot_height, dpi = 300, limitsize = FALSE)
