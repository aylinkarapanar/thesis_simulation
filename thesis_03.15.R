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
#can be useful in comparing the fit indices 
#config_vs_metric <- compareFit(cfa_config, cfa_metric)
#summary(config_vs_metric)

#
set.seed(123)
n <- 100

group1_parameters <- " latent =~ item1 + item2 + item3 + item4"
group2_parameters <- " latent =~ item1 + item2 + item3 + 0.5*item4"

metric_data_group1 <- simulateData(model = group1_parameters, sample.nobs = n)
metric_data_group2 <- simulateData(model = group2_parameters, sample.nobs = n)

metric_data_group1$group <- "Group1"
metric_data_group2$group <- "Group2"

metric_data <- rbind(metric_data_group1, metric_data_group2)

model <- "latent =~ item1 + item2 + item3 + item4"

cfa_metric <- cfa(model, data = metric_data , group = "group", group.equal = "loadings")
fit_measures <-fitMeasures(cfa_metric, c("chisq", "df", "pvalue", "cfi", "rmsea"))


# Printing fit measures
print(fit_measures)
# Summary of the CFA
#summary(cfa_metric, fit.measures = TRUE, standardized = TRUE)


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



