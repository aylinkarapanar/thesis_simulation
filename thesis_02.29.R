#install.packages('semTools')
#install.packages('lavaan')

library(lavaan)
library(semTools)

###########
#Configural Measurement 
set.seed(123)
n <- 100

population_parameters <- " latent =~ item1 + item2 + item3 + item4"

config_data_group1 <- simulateData(model = population_parameters, sample.nobs = n)

config_data_group2 <- simulateData(model = population_parameters, sample.nobs = n)

config_data_group1$group <- "Group1"
config_data_group2$group <- "Group2"

config_data <- rbind(config_data_group1, config_data_group2)

model <- "
    latent =~ item1 + item2 + item3 + item4"

config_data <- rbind(config_data_group1, config_data_group2)

cfa_config <- cfa(model, data = config_data , group = "group")

summary(cfa_config, fit.measures = TRUE, standardized = TRUE)

##############
#Measure (Week) Invariance

set.seed(123)
n <- 100

group1_parameters <- " latent =~ item1 + item2 + item3 + item4"
group2_parameters <- " latent =~ item1 + item2 + item3 + 0.5*item4"

metric_data_group1 <- simulateData(model = group1_parameters, sample.nobs = n)

metric_data_group2 <- simulateData(model = group2_parameters, sample.nobs = n)

metric_data_group1$group <- "Group1"
metric_data_group2$group <- "Group2"

metric_data <- rbind(metric_data_group1, metric_data_group2)

model <- "
    latent =~ item1 + item2 + item3 + item4"

metric_data <- rbind(metric_data_group1, metric_data_group2)

cfa_metric <- cfa(model, data = metric_data , group = "group", group.equal = "loadings")

summary(cfa_metric, fit.measures=T, standardized = T)
#can me useful in comparing the fit indices 
#config_vs_metric <- compareFit(cfa_config, cfa_metric)
#summary(config_vs_metric)

###########
#Scalar (Strong) Invariance
set.seed(123)
n <- 100

#intercepts_group1 <- c(1, 1, 1, 1)  # All intercepts set to 1 for Group 1
#intercepts_group2 <- c(1, 1, 1, 0.5)  # Intercepts set to 1 for items 1, 2, 3 and 0.5 for item 4 for Group 2

scalar_model_group1 <- '
    latent =~ item1 + item2 + item3 + item4
    #intercepts
    item1 ~ 0
    item2 ~ 0
    item3 ~ 0
    item4 ~ 0
    
     # intercepts constrained to be equal
  # using the default names
    #item1 ~ 1
    #item2 ~ equal("item1") * 1
    #item3 ~ equal("item1") * 1
    #item4 ~ equal("item1") * 1
    
    #item1 ~ int1*1
    #item2 ~ int2*1
    #item3 ~ int3*1
    #item4 ~ int4*1
    #int1 == 1
    #int2 == 1
    #int3 == 1
    #int4 == 1
'
scalar_model_group2 <- '
    latent =~ item1 + item2 + item3 + item4
    #intercepts
    item1 ~ 0
    item2 ~ 0
    item3 ~ 0
    item4 ~ 1
    #item1 ~ 1
    #item2 ~ equal("item1") * 1
    #item3 ~ equal("item1") * 1
    #item4 ~ equal("item1") * 0.5
    #item1 + item2 + item3 + item4 ~ 0.5
'

intercept_data_group1 <- simulateData(model = scalar_model_group1, sample.nobs = n)
#Intercepts:
#Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#.item1   (.10.)   -0.166    0.112   -1.480    0.139   -0.166   -0.128
#.item2   (.11.)   -0.097    0.111   -0.874    0.382   -0.097   -0.077
#.item3   (.12.)   -0.049    0.127   -0.384    0.701   -0.049   -0.035
#.item4   (.13.)   -0.089    0.122   -0.736    0.462   -0.089   -0.066

intercept_data_group2 <- simulateData(model = scalar_model_group2, sample.nobs = n)
#Intercepts:
#  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#.item1   (.10.)   -0.166    0.112   -1.480    0.139   -0.166   -0.132
#.item2   (.11.)   -0.097    0.111   -0.874    0.382   -0.097   -0.071
#.item3   (.12.)   -0.049    0.127   -0.384    0.701   -0.049   -0.033
#.item4   (.13.)   -0.089    0.122   -0.736    0.462   -0.089   -0.060
#latent           -0.010    0.141   -0.071    0.943   -0.011   -0.011

intercept_data_group1$group <- "Group1"
intercept_data_group2$group <- "Group2"

intercept_data <- rbind(intercept_data_group1, intercept_data_group2)

model <- "
    latent =~ item1 + item2 + item3 + item4"
cfa_scalar <- cfa(model, data = intercept_data , group = "group", group.equal = c("loadings", "intercepts"))

summary(cfa_scalar, fit.measures = TRUE, standardized = TRUE)

#
#########
#Strict Invariance
set.seed(123)
n <- 100

residuals_model_group1 <- '
    latent =~ item1 + item2 + item3 + item4
    #residual (co)variances
    item1 ~~ item1, value = 0
    item2 ~~ item2, value = 0
    item3 ~~ item3, value = 0
    item4 ~~ item4, value = 0
'
residuals_model_group2 <- '
    latent =~ item1 + item2 + item3 + item4
     #residual (co)variances
    item1 ~~ item1, value = 0
    item2 ~~ item2, value = 0
    item3 ~~ item3, value = 0
    item4 ~~ item4, value = 1
'

residuals_data_group1 <- simulateData(model = residuals_model_group1, sample.nobs = n, ov.var = intercepts_group1)

residuals_data_group2 <- simulateData(model = residuals_model_group2, sample.nobs = n, ov.var = intercepts_group2)

residuals_data_group1$group <- "Group1"
residuals_data_group2$group <- "Group2"

residuals_data <- rbind(residuals_data_group1, residuals_data_group2)
model <- "
    latent =~ item1 + item2 + item3 + item4"

strict <- cfa(model, data = residuals_data , group = "group" ,
              group.equal = c("loadings", "intercepts", "residuals"))

#####################
library(lavaan)

# Set seed for reproducibility
set.seed(123)

# Define different residual variances for each group
residual_variances_group1 <- c(1, 1, 1, 1)
residual_variances_group2 <- c(0.2, 0.3, 0.4, 0.5)  # Different residual variances for Group 2

# Define the model syntax for each group with different residual variances
model_syntax_group1 <- '
    latent =~ item1 + item2 + item3 + item4
    # Residual variances for Group 1
    item1 ~~ item1
    item2 ~~ item2
    item3 ~~ item3
    item4 ~~ item4
'

model_syntax_group2 <- '
    latent =~ item1 + item2 + item3 + item4
    # Residual variances for Group 2
    item1 ~~ item1
    item2 ~~ item2
    item3 ~~ item3
    item4 ~~ item4
'

# Simulate data for each group with the specified model syntax
n <- 100  # Sample size
residuals_data_group1 <- simulateData(model = model_syntax_group1, sample.nobs = n)
residuals_data_group2 <- simulateData(model = model_syntax_group2, sample.nobs = n)

# Update the residual variances in the data for Group 2
for (i in 1:4) {
  residuals_data_group2[, paste0("item", i)] <- rnorm(n, mean = residuals_data_group2[, paste0("item", i)],
                                                      sd = sqrt(residual_variances_group2[i]))
}

# Set group labels
residuals_data_group1$group <- "Group1"
residuals_data_group2$group <- "Group2"

# Combine data from both groups
residuals_data <- rbind(residuals_data_group1, residuals_data_group2)

# Fit the model with strict measurement invariance
model <- "
    latent =~ item1 + item2 + item3 + item4"
strict <- cfa(model, data = residuals_data, group = "group", 
              group.equal = c("loadings", "intercepts", "residuals"))

# Print summary
summary(strict, fit.measures = TRUE, standardized = TRUE)
