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
  sample_size = numeric(),
  noninvariance_ratio = numeric(),
  magnitude_level = numeric(),
  chisq = numeric(),
  pvalue = numeric(),
  cfi = numeric(),
  rmsea = numeric()
)

set.seed(123)
n <- c(50, 100, 250, 500)
factor_loadings <- c(0.9, 0.8, 0.7, 0.6)
models <- c("latent =~ item1 + item2 + item3 + factor_loading*item4", 
            "latent =~ item1 + item2 + factor_loading*item3 + factor_loading*item4",  
            "latent =~ item1 + factor_loading*item2 + factor_loading*item3 + factor_loading*item4", 
            "latent =~ factor_loading*item1 + factor_loading*item2 + factor_loading*item3 + factor_loading*item4")
ratios <- c(0.25, 0.5, 0.75, 1)
iterations <- 100

for (size in n) {
  for (i in 1:4) {
    model <- models[i]
    for (loading in factor_loadings) {
      for (iter in 1:iterations) {
        
      group1_parameters <- " latent =~ item1 + item2 + item3 + item4"
      group2_parameters <- model
      
      metric_data_group1 <- simulateData(model = group1_parameters, sample.nobs = n)
      metric_data_group2 <- simulateData(model = group2_parameters, sample.nobs = n)
      
      metric_data_group1$group <- "Group1"
      metric_data_group2$group <- "Group2"
      
      metric_data <- rbind(metric_data_group1, metric_data_group2)
      
      model <- "latent =~ item1 + item2 + item3 + item4"
      
      cfa_metric <- cfa(model, data = metric_data , group = "group", group.equal = "loadings")
      fit_measures <-fitMeasures(cfa_metric, c("chisq", "pvalue", "cfi", "rmsea"))
      
      temp_df <- data.frame(
        sample_size = size*2,
        noninvarience_ratio = ratios[i],
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
}

#
print(results_df)

#changing the sample_size and magnitude from integer to factor, so that anova can be performed
results_df$sample_size <- factor(results_df$sample_size, levels = c("100", "200", "500", "1000"))
results_df$magnitude_level <- factor(results_df$magnitude_level, levels = c("0.9", "0.8", "0.7", "0.6"))
results_df$noninvarience_ratio <- factor(results_df$noninvarience_ratio, levels = c("0.25", "0.5", "0.75", "1"))

anova_chisq <- aov(chisq ~ sample_size * magnitude_level * noninvarience_ratio,
           data = results_df
)
summary(anova_chisq)

anova_rmsea <- aov(rmsea ~ sample_size * magnitude_level * noninvarience_ratio,
                   data = results_df
)

summary(anova_rmsea)

anova_cfi <- aov(rmsea ~ sample_size * magnitude_level * noninvarience_ratio,
                 data = results_df
)

summary(anova_cfi)

#Scalar (Strong) Invariance
set.seed(123)
n <- 100

scalar_model_group1 <- '
    latent =~ item1 + item2 + item3 + item4
    #intercepts
    item1 ~ 0
    item2 ~ 0
    item3 ~ 0
    item4 ~ 0
'
#scalar_model_group2 <- '
#    latent =~ item1 + item2 + item3 + item4
    #intercepts
#    item1 ~ 0
#    item2 ~ 0
#    item3 ~ 0
#    item4 ~ 1*1
#'
factor_loading <- 0.5
deneme <- c("latent =~ item1 + item2 + factor_loading*item3 + factor_loading*item4", "latent =~ item1 + item2 + factor_loading*item3 + factor_loading*item4")

intercept_data_group1 <- simulateData(model = scalar_model_group1, sample.nobs = n)

intercept_data_group2 <- simulateData(model = deneme[3], sample.nobs = n)


intercept_data_group1$group <- "Group1"
intercept_data_group2$group <- "Group2"

intercept_data <- rbind(intercept_data_group1, intercept_data_group2)

model <- "
    latent =~ item1 + item2 + item3 + item4"

#to investigate the values of the intercepts run this 
cfa_metric <- cfa(model, data = intercept_data , group = "group", group.equal = c("loadings"), std.lv=T)
summary(cfa_metric, fit.measures = TRUE, standardized = TRUE)

cfa_scalar <- cfa(model, data = intercept_data , group = "group", group.equal = c("loadings", "intercepts"), std.lv=T)
summary(cfa_scalar, fit.measures = TRUE, standardized = TRUE)


#anova(cfa_metric, cfa_scalar)

