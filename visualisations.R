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

# calculating mean, standard error, and confidence intervals for Chi-square
mean_chisq <- results %>%
  group_by(model_ratios, magnitude_level, group_size) %>%
  summarise(mean = mean(chisq),
            se = sd(chisq) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

chisq_line_plot <- ggplot(mean_chisq, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(width = 0.5)) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Graph of the Mean of Chi-Square Values with 95% Confidence Interval",
       subtitle = "by Sample Size Per Group, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Size Per Group",
       y = "Chi-Square") +
  theme(legend.position = "bottom")  

chisq_file_path <- file.path(directory, "chisq_line_plot.png")
ggsave(chisq_file_path, plot = chisq_line_plot, width = 12, height = 6)
         
# calculating mean, standard error, and confidence intervals for RMSEA
mean_rmsea <- results %>%
  group_by(model_ratios, magnitude_level, group_size) %>%
  summarise(mean = mean(rmsea),
            se = sd(rmsea) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

rmsea_line_plot <- ggplot(mean_rmsea, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(width = 0.5)) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Graph of the Mean of RMSEA Values with 95% Confidence Interval",
       subtitle = "by Sample Size Per Group, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Size Per Group",
       y = "RMSEA") +
  theme(legend.position = "bottom")  

rmsea_file_path <- file.path(directory, "rmsea_line_plot.png")
ggsave(rmsea_file_path, plot = rmsea_line_plot, width = 12, height = 6)

# calculating mean, standard error, and confidence intervals for CFI
mean_cfi <- results %>%
  group_by(model_ratios, magnitude_level, group_size) %>%
  summarise(mean = mean(cfi),
            se = sd(cfi) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)


cfi_line_plot <- ggplot(mean_cfi, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(width = 0.5)) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_minimal() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Graph of the Mean of CFI Values with 95% Confidence Interval",
       subtitle = "by Sample Size Per Group, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Size Per Group",
       y = "CFI") +
  theme(legend.position = "bottom")  

cfi_file_path <- file.path(directory, "cfi_line_plot.png")
ggsave(cfi_file_path, plot = cfi_line_plot, width = 12, height = 6)


