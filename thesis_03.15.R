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
  cat(paste(c(
    "item1 ~ 0", "item2 ~ 0", "item3 ~ 0")), paste("item4 ~ 1*", intercept), sep = "\n")
}

#############
#Scalar (Strong) Invariance
start_time <- Sys.time()

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
intercepts <- c(0.9, 0.8, 0.7, 0.6)
#try with large sample
models <- c("latent =~ item1 + item2 + item3 + item4
            #intercepts
            item1 ~ 0
            item2 ~ 0
            item3 ~ 0
            item4 ~ 1*0.9")
            #"latent =~ item1 + item2 + factor_loading*item3 + loading*item4",  
            #"latent =~ item1 + loading*item2 + loading*item3 + loading*item4", 
            #"latent =~ loading*item1 + loading*item2 + loading*item3 + loading*item4")
ratios <- c(0.25, 0.5, 0.75, 1)
iterations <- 2

for (size in n) {
  #for (i in 1:4) {
  #model <- model[i]
  for (intercept in intercepts) {
    for (iter in 1:iterations) {
        
        group1_parameters <- " latent =~ item1 + item2 + item3 + item4"
        group2_parameters <-   cat(paste(c(
          "item1 ~ 0", "item2 ~ 0", "item3 ~ 0")), paste("item4 ~ 1*", intercept), sep = "\n")
        
        #missing quote at the end
        
        scalar_data_group1 <- simulateData(model = group1_parameters, sample.nobs = size)
        scalar_data_group2 <- simulateData(model = group2_parameters, sample.nobs = size)
        
        scalar_data_group1$group <- "Group1"
        scalar_data_group2$group <- "Group2"
        
        scalar_data <- rbind(scalar_data_group1, scalar_data_group2)
        
        population_parameters <- "latent =~ item1 + item2 + item3 + item4"
        
        cfa_metric <- cfa(population_parameters, data = scalar_data , group = "group", group.equal = "loadings")
        print(intercept)
        print(summary(cfa_metric))
        cfa_scalar <- cfa(population_parameters, data = scalar_data , group = "group", group.equal = c("loadings", "intercepts"))
        fit_measures <-fitMeasures(cfa_scalar, c("chisq", "pvalue", "cfi", "rmsea"))
        #print(summary(cfa_scalar, fit.measures = TRUE, standardized = TRUE))
        temp_df <- data.frame(
          sample_size = size*2,
          #noninvarience_ratio = ratios[i],
          magnitude_level = intercept,
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


#
print(results_df)

#changing the sample_size and magnitude from integer to factor, so that anova can be performed
results_df$sample_size <- factor(results_df$sample_size, levels = c("100", "200", "500", "1000"))
results_df$magnitude_level <- factor(results_df$magnitude_level, levels = c("0.9", "0.8", "0.7", "0.6"))
#results_df$noninvarience_ratio <- factor(results_df$noninvarience_ratio, levels = c("0.25", "0.5", "0.75", "1"))

anova_chisq <- aov(chisq ~ sample_size * magnitude_level, #* noninvarience_ratio,
                   data = results_df
)
summary(anova_chisq)

anova_rmsea <- aov(rmsea ~ sample_size * magnitude_level, #* noninvarience_ratio,
                   data = results_df
)

summary(anova_rmsea)

anova_cfi <- aov(rmsea ~ sample_size * magnitude_level, #* noninvarience_ratio,
                 data = results_df
)

summary(anova_cfi)

end_time <- Sys.time()
duration <- end_time - start_time
print(duration)



