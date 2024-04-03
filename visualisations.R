#install.packages('ggplot2')
#install.packages('gridExtra')
library(ggplot2)
library(gridExtra)

#chi-square
chisquare_plot <- ggplot(results, aes(x = group_size, y = chisq)) +
  geom_boxplot() + 
  facet_grid(rows = vars(model_ratios), cols = vars(magnitude_level)) + 
  labs(x = "Sample Size", y = "Chi-Square") 

print(chisquare_plot)

#RMSEA
rmsea_plot <- ggplot(results, aes(x = group_size, y = rmsea)) +
  geom_boxplot() + 
  facet_grid(rows = vars(model_ratios), cols = vars(magnitude_level)) + 
  labs(x = "Sample Size", y = "RMSEA") 

print(rmsea_plot)

#CFI
cfi_plot <- ggplot(results, aes(x = group_size, y = cfi)) +
  geom_boxplot() + 
  facet_grid(rows = vars(model_ratios), cols = vars(magnitude_level)) + 
  labs(x = "Sample Size", y = "CFI") 

print(cfi_plot)


#####
#getting the directory, so that the plots can be saved in the same place as the R file
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

custom_colors <- c("0.1" = "#1b9e77", "0.2" = "#d95f02", "0.3" = "#7570b3", "0.4" = "#e7298a")

chisq_color_plot <- ggplot(results, aes(x = group_size, y = chisq, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios)+
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of Chi Square Values",
       subtitle = "by Sample Group Size, Magnitude Level & Model Ratios", 
       x = "Sample Group Size",
       y = "Chi-Square") 

chisq_file_path <- file.path(directory, "chisq_color_plot.png")
ggsave(chisq_file_path, plot = chisq_color_plot, width = 12, height = 6)

print(chisq_color_plot)

rmsea_color_plot <- ggplot(results, aes(x = group_size, y = rmsea, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios)+
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of RMSEA Values",
       subtitle = "by Sample Group Size, Magnitude Level & Model Ratios",
       x = "Sample Group Size",
       y = "RMSEA")


file_path <- file.path(directory, "rmsea_color_plot.jpeg")
ggsave(file_path, rmsea_color_plot, width = 12, height = 6)

print(rmsea_color_plot)

cfi_color_plot <- ggplot(results, aes(x = group_size, y = cfi, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios)+
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of CFI Values",
       subtitle = "by Sample Group Size, Magnitude Level & Model Ratios", 
       x = "Sample Group Size",
       y = "CFI") 

cfi_file_path <- file.path(directory, "cfi_color_plot.png")
ggsave(cfi_file_path, plot = cfi_color_plot, width = 12, height = 6)

##############
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

custom_colors <- c("0.1" = "#1b9e77", "0.2" = "#d95f02", "0.3" = "#7570b3", "0.4" = "#e7298a")

chisq_color_plot <- ggplot(results, aes(x = group_size, y = chisq, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of Chi Square Values",
       subtitle = "by Sample Group Size, Magnitude Level & Model Ratios", 
       x = "Sample Group Size",
       y = "Chi-Square") 

rmsea_color_plot <- ggplot(results, aes(x = group_size, y = rmsea, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of RMSEA Values",
       subtitle = "by Sample Group Size, Magnitude Level & Model Ratios",
       x = "Sample Group Size",
       y = "RMSEA")

cfi_color_plot <- ggplot(results, aes(x = group_size, y = cfi, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of CFI Values",
       subtitle = "by Sample Group Size, Magnitude Level & Model Ratios", 
       x = "Sample Group Size",
       y = "CFI") 


combined_plots <- grid.arrange(chisq_color_plot, rmsea_color_plot, cfi_color_plot, nrow = 3)
combined_file_path <- file.path(directory, "combined_plots.jpeg")
ggsave(combined_file_path, plot = combined_plots, width = 12, height = 18)

