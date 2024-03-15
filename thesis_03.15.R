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


##############
#Measure (Week) Invariance
results_df <- data.frame(
  sample_size = integer(),
  magnitude_level = numeric(),
  chisq = numeric(),
  pvalue = numeric(),
  cfi = numeric(),
  rmsea = numeric()
)

set.seed(123)
n <- c(50, 100, 250, 500)
factor_loadings <- c(0.9, 0.8, 0.7, 0.6)
iterations <- 1000
for (size in n) {
  for (loading in factor_loadings) {
    for (iter in 1:iterations) {group1_parameters <- " latent =~ item1 + item2 + item3 + item4"
    group2_parameters <- " latent =~ item1 + item2 + item3 + factor_loading*item4"
    
    metric_data_group1 <- simulateData(model = group1_parameters, sample.nobs = n)
    metric_data_group2 <- simulateData(model = group2_parameters, sample.nobs = n)
    
    metric_data_group1$group <- "Group1"
    metric_data_group2$group <- "Group2"
    
    metric_data <- rbind(metric_data_group1, metric_data_group2)
    
    model <- "latent =~ item1 + item2 + item3 + item4"
    
    cfa_metric <- cfa(model, data = metric_data , group = "group", group.equal = "loadings")
    fit_measures <-fitMeasures(cfa_metric, c("chisq", "pvalue", "cfi", "rmsea"))
    
    temp_df <- data.frame(
      sample_size = n*2,
      magnitude_level = factor_loadings,
      chisq = fit_measures['chisq'],
      pvalue = fit_measures['pvalue'],
      cfi = fit_measures['cfi'],
      rmsea = fit_measures['rmsea']
    )
    
    # Reset row names
    row.names(temp_df) <- NULL
    
    # Bind the temporary dataframe to results_df
    results_df <- rbind(results_df, temp_df)
    }
  }
}

# Printing the data frame
print(results_df)
# Summary of the CFA
summary(cfa_metric, fit.measures = TRUE, standardized = TRUE)


#Scalar (Strong) Invariance
set.seed(123)
n <- 10000

scalar_model_group1 <- '
    latent =~ item1 + item2 + item3 + item4
    #intercepts
    item1 ~ 0
    item2 ~ 0
    item3 ~ 0
    item4 ~ 0
'
scalar_model_group2 <- '
    latent =~ item1 + item2 + item3 + item4
    #intercepts
    item1 ~ 0
    item2 ~ 0
    item3 ~ 0
    item4 ~ 1*1
'

intercept_data_group1 <- simulateData(model = scalar_model_group1, sample.nobs = n)

intercept_data_group2 <- simulateData(model = scalar_model_group2, sample.nobs = n)


intercept_data_group1$group <- "Group1"
intercept_data_group2$group <- "Group2"

intercept_data <- rbind(intercept_data_group1, intercept_data_group2)

model <- "
    latent =~ item1 + item2 + item3 + item4"

#to investigate the values of the intercepts run this 
#cfa_metric <- cfa(model, data = intercept_data , group = "group", group.equal = c("loadings"), std.lv=T)
#summary(cfa_metric, fit.measures = TRUE, standardized = TRUE)

cfa_scalar <- cfa(model, data = intercept_data , group = "group", group.equal = c("loadings", "intercepts"), std.lv=T)
summary(cfa_scalar, fit.measures = TRUE, standardized = TRUE)


#anova(cfa_metric, cfa_scalar)



