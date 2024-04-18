#install.packages('lavaan')
#install.packages('effectsize')
#install.packages("semTools")
library(lavaan)
library(effectsize)
library(semTools)



#setting seed for reproducibility
set.seed(123)

#defining independent variables and their values
n <- c(50, 100, 250, 500)
intercepts <- c(0.1, 0.2, 0.3, 0.4)
iterations <- 1:1000
ratios <- c(0.25, 0.5, 0.75)

#creating a vector to save the model syntax
models <- c()

#defining models with different noninvariance ratios and intercept values
for (i in 1:length(intercepts)) {
  output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
  output <- paste(output, "item1 ~", c(0, 0, 0), "*1","\n")
  output <- paste(output, "item2 ~", c(0, 0, intercepts[i]), "*1", "\n")
  output <- paste(output, "item3 ~", c(0, intercepts[i], intercepts[i]),"*1", "\n")
  output <- paste(output, "item4 ~", c(intercepts[i], intercepts[i], intercepts[i]),"*1", "\n")
  
  models <- c(models, output)
}

#creating a data frame to store each condition and their iterations
design <- expand.grid(model = models, group_size = n, iteration = iterations)

design$ndesign <- 1:nrow(design)

#creating a data frame for the results
results <- expand.grid(ndesign = design$ndesign,
                       chisq = 0,
                       pvalue = 0,
                       cfi = 0,
                       rmsea = 0)

#recording the noninvariance ratio and intercept values for each model int the results df
model_ratios <- rep(ratios, times = length(intercepts)*length(iterations)*length(n))
results$model_ratios <- model_ratios

magnitude_level <- rep(intercepts, each = length(ratios), times = length(n)*length(iterations))
results$magnitude_level <- magnitude_level

results$group_size <- design$group_size

#defining the model for group 1
group1_string <- "latent =~ item1 + item2 + item3 + item4"

for (i in 1:nrow(design)){
  
  #simulating data for both groups with defined model syntax and sample size
  scalar_data_group1 <- simulateData(model = group1_string, sample.nobs = design$group_size[i])
  scalar_data_group2 <- simulateData(model = design$model[i], sample.nobs = design$group_size[i])
  
  #adding grouping variable for both
  scalar_data_group1$group <- "Group1"
  scalar_data_group2$group <- "Group2"
  
  #adding the data from the groups together
  scalar_data <- rbind(scalar_data_group1, scalar_data_group2)
  
  #executing CFA for weak invariance assessment
  cfa_metric <- cfa(group1_string, data = scalar_data, group = "group", group.equal = "loadings")
  
  #executing CFA for scalar invariance assessment 
  cfa_scalar <- cfa(group1_string, data = scalar_data, group = "group", group.equal = c("loadings", "intercepts"))
  
  compare_cfa <- compareFit(cfa_metric, cfa_scalar)
  
  #recording the fit measures to the results df
  results$chisq[results$ndesign == i] <- compare_cfa@nested$`Chisq diff`[2]
  results$pvalue[results$ndesign == i] <- compare_cfa@nested$`Pr(>Chisq)`[2]
  results$cfi[results$ndesign == i] <- compare_cfa@fit.diff$cfi
  results$rmsea[results$ndesign == i] <- compare_cfa@fit.diff$rmsea
}

#turning these variables to categorical variables for executing ANOVA
results$group_size <- factor(results$group_size, levels = c("50", "100", "250", "500"))
results$magnitude_level <- factor(results$magnitude_level, levels = c("0.1", "0.2", "0.3", "0.4"))
results$model_ratios <- factor(results$model_ratios, levels = c("0.25", "0.5", "0.75", "1"))

#running ANOVAs for each fit indices
anova_chisq <- aov(chisq ~ group_size * magnitude_level * model_ratios, data = results)
summary(anova_chisq)

anova_rmsea <- aov(rmsea ~ group_size * magnitude_level * model_ratios, data = results)
summary(anova_rmsea)

anova_cfi <- aov(cfi ~ group_size * magnitude_level * model_ratios, data = results)
summary(anova_cfi)

#calculating the eta squared and interpreting them based on Field (2013)
eta_chisq <- eta_squared(anova_chisq)
interpret_eta_squared(eta_chisq, rules = "field2013")

eta_rmsea <- eta_squared(anova_rmsea)
interpret_eta_squared(eta_rmsea, rules = "field2013")

eta_cfi <- eta_squared(anova_cfi)
interpret_eta_squared(eta_cfi, rules = "field2013")
