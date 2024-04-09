#install.packages('ggplot2')
#install.packages('gridExtra')
library(ggplot2)
library(gridExtra)


directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

custom_colors <- c("0.1" = "#1b9e77", "0.2" = "#d95f02", "0.3" = "#7570b3", "0.4" = "#e7298a")

chisq_color_plot <- ggplot(results, aes(x = group_size, y = chisq, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of Chi Square Values",
       subtitle = "by Sample Group Size, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Group Size",
       y = "Chi-Square") 

chisq_file_path <- file.path(directory, "chisq_color_plot.png")
ggsave(chisq_file_path, plot = chisq_color_plot, width = 12, height = 6)


rmsea_color_plot <- ggplot(results, aes(x = group_size, y = rmsea, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of RMSEA Values",
       subtitle = "by Sample Group Size, Magnitude Level & Noninvariance Ratios",
       x = "Sample Group Size",
       y = "RMSEA")
file_path <- file.path(directory, "rmsea_color_plot.jpeg")
ggsave(file_path, rmsea_color_plot, width = 12, height = 6)


cfi_color_plot <- ggplot(results, aes(x = group_size, y = cfi, color = magnitude_level)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Boxplot of CFI Values",
       subtitle = "by Sample Group Size, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Group Size",
       y = "CFI") 

cfi_file_path <- file.path(directory, "cfi_color_plot.png")
ggsave(cfi_file_path, plot = cfi_color_plot, width = 12, height = 6)

#combined_plots <- grid.arrange(chisq_color_plot, rmsea_color_plot, cfi_color_plot, nrow = 3)
#combined_file_path <- file.path(directory, "combined_plots.jpeg")
#ggsave(combined_file_path, plot = combined_plots, width = 12, height = 18)



###arrow plot


mean_results <- aggregate(chisq ~ model_ratios + magnitude_level + group_size, data = results, FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  ci_low <- mean_val - 1.96 * se_val
  ci_high <- mean_val + 1.96 * se_val
  c(mean_val, ci_low, ci_high)  # Return mean, lower CI, upper CI
})

# Rename columns for clarity
colnames(mean_results) <- c("model_ratios", "magnitude_level", "group_size", "mean", "ci_low", "ci_high")

# Create the arrow plot with confidence intervals
chisq_arrow_plot <- ggplot(mean_results, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 1.2) +  # Plot mean and confidence intervals
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Arrow Plot of Mean Chi Square Values with Confidence Intervals",
       subtitle = "by Sample Group Size, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Group Size",
       y = "Mean Chi-Square") +
  theme(legend.position = "bottom")  # Adjust legend position if needed

# Print the arrow plot
print(chisq_arrow_plot)
