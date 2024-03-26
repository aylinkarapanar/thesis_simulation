#install.packages('lavaan')
#install.packages('semTools')
#install.packages('rstatix')
#install.packages('sjstats')
#install.packages('tidySEM')

library(rstatix)
library(sjstats)
library(tidySEM)
library(lavaan)
library(semTools)

#try out using grid
#create a data frame with the n of total iterations as rows and different conditions as columns
#write the results in that data frame instead of 

  
intercepts <- c(0.9, 0.8, 0.7, 0.6)
for (intercept in intercepts) {
  cat(paste("\"latent =~ item1 + item2 + item3 + item4"))
  cat(paste(c(
    "item1 ~ 0", "item2 ~ 0", "item3 ~ 0")), paste("item4 ~ 1*", intercept, "\""), sep = "\n")
}


paste("latent =~ item1 + item2 + item3 + item4", c(
  "item1 ~ 0", "item2 ~ 0", "item3 ~ 0"), "item4 ~ 1*", intercept, sep = " ", collapse = "\"")

model_string <- "latent =~ item1 + item2 + item3 + item4"
intercepts <- c("item1 ~ 0", "item2 ~ 0", "item3 ~ 0", paste("item4 ~ 1*", intercept))

# Combine the model string and intercepts, separating them with newline characters
combined_model <- c(model_string, intercepts)
combined_model <- paste(combined_model, collapse = "\n ")

# Print the combined model
print(combined_model)


###############
set.seed(123)
n <- c(50, 100, 250, 500)
intercepts <- c(0.9, 0.8, 0.7, 0.6)

results_df <- data.frame(
  sample_size = numeric(),
  magnitude_level = numeric(),
  chisq = numeric(),
  pvalue = numeric(),
  cfi = numeric(),
  rmsea = numeric()
)


ratios <- c(0.25, 0.5, 0.75, 1)
iterations <- 2

for (size in n) {
  for (intercept in intercepts) {
    for (iter in 1:iterations) {
      
      model_string <- "latent =~ item1 + item2 + item3 + item4"
      
      intercept_part <- c("item1 ~ 0", "item2 ~ 0", "item3 ~ 0", paste("item4 ~ 1*", intercept))
      
      combined_model <- c(model_string, intercept_part)
      
      combined_model <- paste(combined_model, collapse = "\n ")
      
      # print the combined model to investigate
      print(combined_model)
      
      scalar_data_group1 <- simulateData(model = model_string, sample.nobs = size)
      scalar_data_group2 <- simulateData(model = combined_model, sample.nobs = size)
      
      scalar_data_group1$group <- "Group1"
      scalar_data_group2$group <- "Group2"
      
      scalar_data <- rbind(scalar_data_group1, scalar_data_group2)
      
      # print to investigate the intercepts
      cfa_metric <- cfa(model_string, data = scalar_data , group = "group", group.equal = "loadings")
      print(intercept)
      print(summary(cfa_metric)) 
      
      cfa_scalar <- cfa(model_string, data = scalar_data, group = "group", group.equal = c("loadings", "intercepts"))
      fit_measures <- fitMeasures(cfa_scalar, c("chisq", "pvalue", "cfi", "rmsea"))
      
      # create a temporary dataframe
      temp_df <- data.frame(
        sample_size = size * 2,
        magnitude_level = intercept,
        chisq = fit_measures['chisq'],
        pvalue = fit_measures['pvalue'],
        cfi = fit_measures['cfi'],
        rmsea = fit_measures['rmsea']
      )
      
      # bind it to the main results df
      results_df <- rbind(results_df, temp_df)
    }
  }
}

print(results_df)

############
#even when the intercept value is 0.6 and the model syntax is such
#[1] "latent =~ item1 + item2 + item3 + item4\n item1 ~ 0\n item2 ~ 0\n item3 ~ 0\n item4 ~ 1* 0.6"
# the cfa_metric summary comes out as such
#Group 1 [Group1]:

#Intercepts:
#  Estimate  Std.Err  z-value  P(>|z|)
#.item1            -0.083    0.064   -1.306    0.192
#.item2             0.046    0.067    0.690    0.490
#.item3            -0.067    0.065   -1.031    0.303
#.item4            -0.013    0.064   -0.202    0.840

#Group 2 [Group2]:

#Intercepts:
#  Estimate  Std.Err  z-value  P(>|z|)
#.item1            -0.023    0.060   -0.393    0.694
#.item2            -0.080    0.061   -1.324    0.186
#.item3            -0.058    0.063   -0.916    0.360
#.item4             0.959    0.063   15.258    0.000



# changing the sample_size and magnitude from integer to factor for ANOVA
results_df$sample_size <- factor(results_df$sample_size, levels = c("100", "200", "500", "1000"))
results_df$magnitude_level <- factor(results_df$magnitude_level, levels = c("0.9", "0.8", "0.7", "0.6"))

anova_chisq <- aov(chisq ~ sample_size * magnitude_level, data = results_df)
summary(anova_chisq)

anova_rmsea <- aov(rmsea ~ sample_size * magnitude_level, data = results_df)
summary(anova_rmsea)

anova_cfi <- aov(cfi ~ sample_size * magnitude_level, data = results_df)
summary(anova_cfi)

############
for (size in n) {
  for (intercept in intercepts) {
    for (iter in 1:iterations) {
      
      model_string <- "latent =~ item1 + item2 + item3 + item4"
      
      intercept_part <- c("item1 ~ 0", "item2 ~ 0", "item3 ~ 0", paste("item4 ~ 1*", intercept))
      
      combined_model <- c(model_string, intercept_part)
      
      combined_model <- paste(combined_model, collapse = "\n ")
      print(combined_model)
      
      scalar_data_group1 <- simulateData(model = model_string, sample.nobs = size)
      scalar_data_group2 <- simulateData(model = combined_model, sample.nobs = size)
      
      scalar_data_group1$group <- "Group1"
      scalar_data_group2$group <- "Group2"
      
      scalar_data <- rbind(scalar_data_group1, scalar_data_group2)
      
      cfa_model <- cfa(combined_model, data = scalar_data, group = "group", group.equal = c("loadings", "intercepts"))
      
      fit_measures <- fitMeasures(cfa_model, c("chisq", "pvalue", "cfi", "rmsea"))
      cfa_metric <- cfa(model_string, data = scalar_data , group = "group", group.equal = "loadings")
      print(intercept)
      print(summary(cfa_metric)) 
      
      temp_df <- data.frame(
        sample_size = size * 2,
        magnitude_level = intercept,
        chisq = fit_measures['chisq'],
        pvalue = fit_measures['pvalue'],
        cfi = fit_measures['cfi'],
        rmsea = fit_measures['rmsea']
      )
      
      row.names(temp_df) <- NULL
      
      results_df <- rbind(results_df, temp_df)
    }
  }
}

print(results_df)

results_df$sample_size <- factor(results_df$sample_size, levels = c("100", "200", "500", "1000"))
results_df$magnitude_level <- factor(results_df$magnitude_level, levels = c("0.9", "0.8", "0.7", "0.6"))

anova_chisq <- aov(chisq ~ sample_size * magnitude_level, data = results_df)
summary(anova_chisq)

anova_rmsea <- aov(rmsea ~ sample_size * magnitude_level, data = results_df)
summary(anova_rmsea)

anova_cfi <- aov(cfi ~ sample_size * magnitude_level, data = results_df)
summary(anova_cfi)
