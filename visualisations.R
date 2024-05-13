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
  scale_color_manual(values = custom_colors) +
  labs(title = expression(paste("Error Plot of Mean ", italic("p"), " Values for ", Delta, Chi^2, " with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group & Magnitude Level", 
       x = "Sample Size Per Group",
       y = expression(italic("p")),
       color = "Noninvariance \nMagnitude Levels") + 
  theme(legend.position = "right", legend.title = element_text())+ 
  scale_y_continuous(breaks = c(0.0, 0.05, 0.1, 0.2, 0.3, 0.4)) 

pvalue_file_path <- file.path(directory, "pvalue_line_plot.png")
ggsave(pvalue_file_path, plot = pvalue_line_plot, width = 12, height = 6)
         
# calculating mean, standard error, and confidence intervals for RMSEA
# plot for main effect of magnitude level
mean_rmsea <- results %>%
  group_by(magnitude_level) %>%
  summarise(mean = mean(rmsea),
            se = sd(rmsea) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

rmsea_magnitude_level_plot <- ggplot(mean_rmsea, aes(x = magnitude_level, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_apa() +
  labs(title = expression(paste("Error Plot of Mean ", Delta, "RMSEA with 95% Confidence Interval")),
       subtitle = "by Noninvariance Magnitude Level",
       x = "Noninvariance Magnitude Level",
       y = expression(paste(Delta, "RMSEA")))

rmsea_magnitude_file_path <- file.path(directory, "rmsea_magnitude_level_plot.png")
ggsave(rmsea_magnitude_file_path, plot = rmsea_magnitude_level_plot, width = 12, height = 6)


# plot for the main effect of sample size 
mean_rmsea <- results %>%
  group_by(group_size) %>%
  summarise(mean = mean(rmsea),
            se = sd(rmsea) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

rmsea_sample_size_plot <- ggplot(mean_rmsea, aes(x = group_size, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_apa() +
  labs(title = expression(paste("Error Plot of Mean ", Delta, "RMSEA with 95% Confidence Interval")),
       subtitle = "by Sample Size Per Group",
       x = "Sample Size Per Group",
       y = expression(paste(Delta, "RMSEA"))) +
  scale_y_continuous(breaks = c(0.0, 0.010, 0.015, 0.02, 0.03))

# define different cutoff points discussed by Chen (2007)
# for total N ≤ 300, difference in RMSEA ≥ .010 indicates noninvariance
rmsea_sample_size_plot <- rmsea_sample_size_plot +
  annotate("segment", x = 0, xend = 2.5, y = .010, yend = .010, linetype = "dotted") + 
  # for total N > 300, difference in RMSEA ≥ .015 indicates noninvariance
  annotate("segment", x = 2.5, xend = 5, y = .015, yend = .015, linetype = "dotted")

rmsea_sample_size_file_path <- file.path(directory, "rmsea_sample_size_plot.png")
ggsave(rmsea_sample_size_file_path, plot = rmsea_sample_size_plot, width = 12, height = 6)


# calculating mean, standard error, and confidence intervals for CFI
mean_cfi <- results %>%
  group_by(magnitude_level) %>%
  summarise(mean = mean(cfi),
            se = sd(cfi) / sqrt(n()),
            ci_low = mean - 1.96 * se,
            ci_high = mean + 1.96 * se)

cfi_line_plot <- ggplot(mean_cfi, aes(x = magnitude_level, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0) +  
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +  
  theme_apa() +
  labs(title = expression(paste("Error Plot of Mean ", Delta, "CFI with 95% Confidence Interval")),
       x = "Noninvariance Magnitude Level",
       y = expression(paste(Delta, "CFI"))) 

cfi_file_path <- file.path(directory, "cfi_line_plot.png")
ggsave(cfi_file_path, plot = cfi_line_plot, width = 12, height = 6)


###########################################################################################################################################
group1_string <- "latent =~ item1 + item2 + item3 + item4"

#simulating data for both groups with defined model syntax and sample size
dia_data_group1 <- simulateData(model = group1_string, sample.nobs = 50)
dia_data_group2 <- simulateData(model = group1_string, sample.nobs = 50)
  
  #adding grouping variable for both
dia_data_group1$group <- "Group1"
dia_data_group2$group <- "Group2"
  
  #adding the data from the groups together
dia_data <- rbind(dia_data_group1, dia_data_group2)
  
dia_cfa_metric <- cfa(group1_string, data = dia_data, group = "group", group.equal = "loadings")
  
  #executing CFA for scalar invariance assessment 
dia_cfa_scalar <- cfa(group1_string, data = dia_data, group = "group", group.equal = c("loadings", "intercepts"))

dia_cfa_res <- cfa(group1_string, data = dia_data, group = "group", group.equal = c("loadings", "intercepts", "residuals"))
  


node_labels <- c("Item 1", "Item 2", "Item 3", "Item 4", "Factor")
edge_labels <- c("λ", "λ", "λ", "λ")
semPaths(dia_cfa_scalar, intercepts = F, nodeLabels = node_labels, sizeMan = 8, edgeLabels = edge_labels, edge.label.cex = 1)





node_labels <- c("Item 1", "Item 2", "Item 3", "Item 4", "Factor", "v", "v", "v", "v", "v")
edge_labels <- c("λ", "λ", "λ", "λ")
semPaths(dia_cfa_scalar, intercepts = T, sizeMan = 8, edgeLabels = edge_labels, nodeLabels = node_labels, residuals = T, optimizeLatRes = T)



semPaths(dia_cfa_res, intercepts = T, residuals = T,  edge.label.cex = 1, sizeMan = 10)

###########
# Install and load the semPlot package if you haven't already
install.packages("semPlot")
library(semPlot)
install.packages("OpenMx")
library(OpenMx)

# Define manifest and latent variables
manifestVars <- c("Item 1", "Item 2", "Item 3", "Item 4")  # Four observed variables
latentVar <- "Factor"  # One latent factor

# Residual variances
resVars <- mxPath(from = manifestVars, arrows = 2,
                  free = TRUE, values = c(1, 1, 1, 1),
                  labels = paste0("e", 1:4))

# Means with different intercepts
means <- mxPath(from = "one", to = manifestVars,
                arrows = 1,
                free = c(TRUE, TRUE, TRUE, TRUE),
                values = c(1, 2, 3, 4),  # Different intercepts
                labels = paste0("i", 1:4))

# Latent variable variance
latVar <- mxPath(from = "Factor", arrows = 2,
                 free = TRUE, values = 1, labels = "varF1")

# Factor loadings with the lambda symbol
facLoads <- mxPath(from = "Factor", to = manifestVars, arrows = 1,
                   free = TRUE, values = c(1, 1, 1, 1),
                   labels = paste0("λ", 1:4))  # Lambda1, Lambda2, etc.

# Define the model
oneFactorModel <- mxModel("One Factor Model Path Specification", type = "RAM",
                          manifestVars = manifestVars, latentVars = latentVar,
                          resVars, means, latVar, facLoads)

# Generate simulated data
simulatedData <- mxData(observed = mxGenerateData(oneFactorModel, nrows = 100), 'raw')

# Associate the data with the model
oneFactorModel <- mxModel(oneFactorModel, simulatedData)

# Run the model
fit <- mxRun(oneFactorModel)

# Summarize the fit
summary(fit)

# Use semPlot to draw the OpenMx model fit
semPaths(fit, residuals = FALSE, sizeMan = 7, "std", 
         posCol = c("skyblue4", "red"),
         edge.label.cex = 1.2, layout = "circle2")



