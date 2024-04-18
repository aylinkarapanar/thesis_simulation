#install.packages('ggplot2')
#install.packages('gridExtra')
#install.packages('dplyr')
#install.packages('jtools')
library(ggplot2)
library(gridExtra)
library(dplyr)
library(jtools)

directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

custom_colors <- c("0.1" = "#1b9e77", "0.2" = "#d95f02", "0.3" = "#7570b3", "0.4" = "#e7298a")



#ADD CUT OFFS AS LINES

# calculating mean, standard error, and confidence intervals for Chi-square
mean_chisq <- results %>%
  group_by(model_ratios, magnitude_level, group_size) %>%
  summarise(mean = mean(chisq),
            se = sd(chisq) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

chisq_line_plot <- ggplot(mean_chisq, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_apa() +
  facet_wrap(~model_ratios) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Graph of the Mean of Chi-Square Values with 95% Confidence Interval",
       subtitle = "by Sample Size Per Group, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Size Per Group",
       y = "Difference in Chi-Square") +
  theme(legend.position = "bottom")  

chisq_file_path <- file.path(directory, "chisq_line_plot.png")
ggsave(chisq_file_path, plot = chisq_line_plot, width = 12, height = 6)
         
# calculating mean, standard error, and confidence intervals for RMSEA
mean_rmsea <- results %>%
  group_by(magnitude_level, group_size) %>%
  summarise(mean = mean(rmsea),
            se = sd(rmsea) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

rmsea_line_plot <- ggplot(mean_rmsea, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, position = position_dodge(width = 0.5)) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  #define different cutoff points discussed by Chen (2007)
  #for total N ≤ 300, difference in RMSEA ≥ .010 indicates noninvariance
  #for total N > 300, difference in RMSEA ≥ .015 indicates noninvariance
  #geom_segment(aes(x = 0, xend = 2.5, y = .010, yend = .010), 
  #             linetype = "dotted", color = "black") +
  #geom_segment(aes(x = 2.5, xend = 5, y = .015, yend = .015), 
  #             linetype = "dotted", color = "black") +
  theme_apa() +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Graph of the Mean of (\0xCE \0x94) RMSEA with 95% Confidence Interval",
       subtitle = "by Sample Size Per Group, Magnitude Level", 
       x = "Sample Size Per Group",
       y = "0xCE 0x94 RMSEA") +
  theme(legend.position = "bottom")

rmsea_line_plot <- rmsea_line_plot + annotate("segment", x = 0, xend = 2.5, y = .010, yend = .010, linetype = "dotted", color = "black") + annotate("segment", x = 2.5, xend = 5, y = .015, yend = .015, linetype = "dotted", color = "black")
 
rmsea_file_path <- file.path(directory, "rmsea_line_plot.png")
ggsave(rmsea_file_path, plot = rmsea_line_plot, width = 12, height = 6)

# calculating mean, standard error, and confidence intervals for CFI
mean_cfi <- results %>%
  group_by(magnitude_level, group_size) %>%
  summarise(mean = mean(cfi),
            se = sd(cfi) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)


cfi_line_plot <- ggplot(mean_cfi, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, position = position_dodge(width = 0.5)) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_apa() +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = "Graph of the Mean of CFI Values with 95% Confidence Interval",
       subtitle = "by Sample Size Per Group, Magnitude Level & Noninvariance Ratios", 
       x = "Sample Size Per Group",
       y = "Difference in CFI") +
  theme(legend.position = "bottom")  

cfi_file_path <- file.path(directory, "cfi_line_plot.png")
ggsave(cfi_file_path, plot = cfi_line_plot, width = 12, height = 6)


