#visualisations

library(ggplot2)

#chi-square
chisquare_plot <- ggplot(results, aes(x = group_size, y = chisq)) +
  geom_point() + 
  facet_grid(rows = vars(model_ratios), cols = vars(magnitude_level)) + 
  labs(x = "Sample Size", y = "Chi-Square") 

print(chisquare_plot)

#RMSEA
rmsea_plot <- ggplot(results, aes(x = group_size, y = rmsea)) +
  geom_point() + 
  facet_grid(rows = vars(model_ratios), cols = vars(magnitude_level)) + 
  labs(x = "Sample Size", y = "RMSEA") 

print(rmsea_plot)

#CFI
cfi_plot <- ggplot(results, aes(x = group_size, y = cfi)) +
  geom_point() + 
  facet_grid(rows = vars(model_ratios), cols = vars(magnitude_level)) + 
  labs(x = "Sample Size", y = "CFI") 

print(cfi_plot)


###boxplots
install.packages('lattice')
library(lattice)

# Create boxplots with interactions
bwplot(chisq ~ group_size | magnitude_level * model_ratios, data = results,
       main = "Boxplot of Chi-Square Metric by Sample Size, Magnitude Level, and Ratio",
       xlab = "Sample Size", ylab = "Chi-Square Metric")
