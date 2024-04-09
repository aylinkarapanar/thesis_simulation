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

#install.packages('dplyr')
library(dplyr)

# Calculate mean, standard error, and confidence intervals
mean_results <- results %>%
  group_by(model_ratios, magnitude_level, group_size) %>%
  summarise(mean = mean(chisq),
            se = sd(chisq) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

# Print the head of mean_results to verify
head(mean_results)

library(ggplot2)

# Create the line graph with error bars and dots for mean
chisq_line_plot <- ggplot(mean_results, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  # Error bars
  geom_point(size = 3) +  # Dots for mean
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Line Graph of Mean Chi Square Values with Confidence Intervals",
       subtitle = "by Sample Group Size, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Group Size",
       y = "Mean Chi-Square") +
  theme(legend.position = "bottom")  # Adjust legend position if needed

# Save the plot to a file
chisq_file_path <- file.path(directory, "chisq_line_plot.png")
ggsave(chisq_file_path, plot = chisq_line_plot, width = 12, height = 6)



