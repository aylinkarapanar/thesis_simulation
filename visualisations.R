#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('jtools')

library(ggplot2)
library(dplyr)
library(jtools)

directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

#setting custom colors
custom_colors <- c("0.1" = "#1b9e77", "0.2" = "#d95f02", "0.3" = "#7570b3", "0.4" = "#e7298a")

# calculating mean, standard error, and confidence intervals for Chi-square
mean_chisq <- results %>%
  group_by(magnitude_level, group_size) %>%
  summarise(mean = mean(chisq),
            se = sd(chisq) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

chisq_line_plot <- ggplot(mean_chisq, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_apa() +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = expression(paste("Graph of the Mean of ", Delta, Chi^2," with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group & Magnitude Level", 
       x = "Sample Size Per Group",
       y = expression(paste(Delta, Chi^2))) +
  theme(legend.position = "bottom")  

chisq_file_path <- file.path(directory, "chisq_line_plot.png")
ggsave(chisq_file_path, plot = chisq_line_plot, width = 12, height = 6)

#p-values
mean_p <- results %>%
  group_by(magnitude_level, group_size) %>%
  summarise(mean = mean(pvalue),
            se = sd(pvalue) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

pvalue_line_plot <- ggplot(mean_p, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  theme_apa() +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = expression(paste("Graph of the Mean of ", italic("p"), "Values for ", Chi^2, " with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group & Magnitude Level", 
       x = "Sample Size Per Group",
       y = expression(italic("p"))) +
  theme(legend.position = "bottom")  

pvalue_line_plot <- pvalue_line_plot + scale_y_continuous(breaks = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.4))
pvalue_file_path <- file.path(directory, "pvalue_line_plot.png")
ggsave(pvalue_file_path, plot = pvalue_line_plot, width = 12, height = 6)
         
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
  theme_apa() +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = expression(paste("Graph of the Mean of ", Delta, "RMSEA with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group & Magnitude Level", 
       x = "Sample Size Per Group",
       y = expression(paste(Delta, "RMSEA"))) +
  theme(legend.position = "bottom")

#define different cutoff points discussed by Chen (2007)
#for total N ≤ 300, difference in RMSEA ≥ .010 indicates noninvariance
rmsea_line_plot <- rmsea_line_plot + annotate("segment", x = 0, xend = 2.5, y = .010, yend = .010, linetype = "dotted", color = "black") + 
#for total N > 300, difference in RMSEA ≥ .015 indicates noninvariance
annotate("segment", x = 2.5, xend = 5, y = .015, yend = .015, linetype = "dotted", color = "black") +
scale_y_continuous(breaks = c(0.0, 0.010, 0.015, 0.02, 0.04, 0.06))
 
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
  labs(title = expression(paste("Graph of the Mean of ", Delta, "CFI with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group & Magnitude Level", 
       x = "Sample Size Per Group",
       y = expression(paste(Delta, "CFI"))) +
  theme(legend.position = "bottom")  

#define different cutoff points discussed by Chen (2007)
#for total N ≤ 300, difference in CFI ≥ -.005 indicates noninvariance
cfi_line_plot <- cfi_line_plot + annotate("segment", x = 0, xend = 2.5, y = -.005, yend = -.005, linetype = "dotted", color = "black") + 
#for total N > 300, difference in CFI ≥ -.010 indicates noninvariance
annotate("segment", x = 2.5, xend = 5, y = -.010, yend = -.010, linetype = "dotted", color = "black") +
  scale_y_continuous(breaks = c(-0.02, -0.010, -0.005))

cfi_file_path <- file.path(directory, "cfi_line_plot.png")
ggsave(cfi_file_path, plot = cfi_line_plot, width = 12, height = 6)


