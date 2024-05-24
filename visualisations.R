#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('jtools')
#install.packages("semPlot")

library(ggplot2)
library(dplyr)
library(jtools)
library(semPlot)

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
ggsave(chisq_file_path, plot = chisq_line_plot)

#p-values
mean_p <- results %>%
  group_by(magnitude_level, group_size) %>%
  summarise(mean = mean(pvalue),
            se = sd(pvalue) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

pvalue_line_plot <- ggplot(mean_p, aes(x = group_size, y = mean, color = magnitude_level)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5), size = 2) +  
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  theme_apa(legend.pos = "bottom", legend.use.title = T, 
            legend.font.size = 14,
            x.font.size = 14,
            y.font.size = 14,
            facet.title.size = 14) +
  theme(axis.text = element_text(size = 12))+
  scale_color_manual(values = custom_colors) +
  labs(title = expression(paste("Error Plot of Mean ", italic("p"), " Values for ", Delta, Chi^2, " with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group & Magnitude Level", 
       x = "Sample Size Per Group",
       y = expression(italic("p")),
       color = "Noninvariance \nMagnitude Levels") + 
  scale_y_continuous(breaks = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.4)) 

pvalue_file_path <- file.path(directory, "pvalue_line_plot.png")
ggsave(pvalue_file_path, plot = pvalue_line_plot)
         
# calculating mean, standard error, and confidence intervals for RMSEA
# plot for main effect of magnitude level
mean_rmsea <- results %>%
  group_by(magnitude_level) %>%
  summarise(mean = mean(rmsea),
            se = sd(rmsea) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

rmsea_magnitude_level_plot <- ggplot(mean_rmsea, aes(x = magnitude_level, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  geom_point(position = position_dodge(width = 0.5), size = 2) +  
  theme_apa(x.font.size = 14,
            y.font.size = 14,
            facet.title.size = 14) +
  theme(axis.text = element_text(size = 12)) +
  labs(title = expression(paste("Error Plot of Mean ", Delta, "RMSEA with 95% Confidence Interval")),
       subtitle = "by Noninvariance Magnitude Level",
       x = "Noninvariance Magnitude Level",
       y = expression(paste(Delta, "RMSEA"))) +
  scale_y_continuous(breaks = c(0.010, 0.015, 0.02, 0.03, 0.04, 0.05))


# define the cutoff point discussed by Chen (2007)
rmsea_magnitude_level_plot  <- rmsea_magnitude_level_plot +
  # for total N > 300, difference in RMSEA ≥ .015 indicates noninvariance
  annotate("segment", x = 0, xend = 5, y = .015, yend = .015, linetype = "dotted")

rmsea_magnitude_file_path <- file.path(directory, "rmsea_magnitude_level_plot.png")
ggsave(rmsea_magnitude_file_path, plot = rmsea_magnitude_level_plot)


# plot for the main effect of sample size 
mean_rmsea <- results %>%
  group_by(group_size) %>%
  summarise(mean = mean(rmsea),
            se = sd(rmsea) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

rmsea_sample_size_plot <- ggplot(mean_rmsea, aes(x = group_size, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  geom_point(position = position_dodge(width = 0.5), size = 2) +  
  theme_apa(x.font.size = 14,
            y.font.size = 14,
            facet.title.size = 14) +
  theme(axis.text = element_text(size = 12))+
  labs(title = expression(paste("Error Plot of Mean ", Delta, "RMSEA with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group",
       x = "Sample Size Per Group",
       y = expression(paste(Delta, "RMSEA"))) +
  scale_y_continuous(breaks = c(0.010, 0.015, 0.02, 0.03, 0.04, 0.05))

# define different cutoff points discussed by Chen (2007)
# for total N ≤ 300, difference in RMSEA  .010 indicates noninvariance
rmsea_sample_size_plot <- rmsea_sample_size_plot +
  annotate("segment", x = 0, xend = 2.5, y = .010, yend = .010, linetype = "dotted") + 
  # for total N > 300, difference in RMSEA ≥ .015 indicates noninvariance
  annotate("segment", x = 2.5, xend = 5, y = .015, yend = .015, linetype = "dotted")

rmsea_sample_size_file_path <- file.path(directory, "rmsea_sample_size_plot.png")
ggsave(rmsea_sample_size_file_path, plot = rmsea_sample_size_plot)


# calculating mean, standard error, and confidence intervals for CFI
mean_cfi <- results %>%
  group_by(magnitude_level) %>%
  summarise(mean = mean(cfi),
            se = sd(cfi) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

cfi_line_plot <- ggplot(mean_cfi, aes(x = magnitude_level, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +  
  geom_point(position = position_dodge(width = 0.5), size = 2) +  
  theme_apa(x.font.size = 14,
            y.font.size = 14,
            facet.title.size = 14) +
  theme(axis.text = element_text(size = 12))+
  labs(title = expression(paste("Error Plot of Mean ", Delta, "CFI with 95% Confidence Interval")),
       x = "Noninvariance Magnitude Level",
       y = expression(paste(Delta, "CFI"))) 

# define the cutoff point discussed by Chen (2007)
cfi_line_plot <- cfi_line_plot +
  #for total N > 300, difference in CFI ≥ -.010 indicates noninvariance
  annotate("segment", x = 0, xend = 5, y = -.010, yend = -.010, linetype = "dotted")

cfi_file_path <- file.path(directory, "cfi_line_plot.png")
ggsave(cfi_file_path, plot = cfi_line_plot)


###########################################################################################################################################
#Model Diagram


node_labels <- c("Item 1", "Item 2", "Item 3", "Item 4", "Factor", "1",  "1", "1", "1", "1")

edge_labels <- c(expression(paste(Λ[11])), expression(paste(Λ[21])), expression(paste(Λ[31])), expression(paste(Λ[41])),
                 expression(paste(ε[1])), expression(paste(ε[2])), expression(paste(ε[3])), expression(paste(ε[4])),
                 expression(paste(Ψ[11])), expression(paste(v[1])), expression(paste(v[2])), expression(paste(v[3])), expression(paste(v[4])))

semPaths(cfa_scalar, intercepts = TRUE, sizeMan = 8, edgeLabels = edge_labels, nodeLabels = node_labels, edge.label.cex = 1, residuals = TRUE, optimizeLatRes = TRUE, filetype = "png", filename = "diagram")