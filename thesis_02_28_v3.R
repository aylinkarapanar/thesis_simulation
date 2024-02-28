library(lavaan)
install.packages('semTools')
library(semTools)
#data with measurment invariance 
set.seed(123)
n <- 100
num_variables <- 4

group1_data <- data.frame(
  matrix(sample(1:5, n * num_variables, replace = TRUE), nrow = n)
)

colnames(group1_data) <- paste0("item", 1:num_variables)

group2_data <- group1_data

# Define the population parameters for the CFA model
model <- "
    latent =~ item1 + item2 + item3 + item4"

group1_data$group <- "Group1"
group2_data$group <- "Group2"

model_data_all <- rbind(group1_data, group2_data)

cfa.config <- cfa(model, data = model_data_all , group = "group")

summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

cfa.scalar <- cfa(model, data = model_data_all, group = "group", 
                  group.equal = c("loadings","intercepts"))
summary(cfa.scalar, fit.measures = TRUE, standardized = TRUE)

measurement_model_group1 <- "
    latent =~ item1 + item2 + item3 + item4"

measurement_model_group2 <- "
    latent =~ item1 + item2 + item3 + 0.75*item4"

cfa_group1 <- cfa(measurement_model_group1, data = group1_data)
cfa_group2 <- cfa(measurement_model_group2, data = group2_data)


# Compare fit indices between the two models
fitmeasures(cfa_group1)
fitmeasures(cfa_group2)

########

population.model <- ' f1 =~ x1 + x2 + x3 + x4'

# generate data
set.seed(1234)
myData <- simulateData(population.model, sample.nobs=100L)

# population moments
fitted(sem(population.model))

# sample moments
round(cov(myData), 3)
round(colMeans(myData), 3)

# fit model
myModel <- ' f1 =~ x1 + x2 + x3+ x4' 

fit <- sem(myModel, data=myData)
summary(fit)

#######
#without having them on the likert scale 
#same as before
set.seed(123)
n <- 100

population_parameters <- " latent =~ item1 + item2 + item3 + item4"

config_data_group1 <- simulateData(model = population_parameters, sample.nobs = n)

config_data_group2 <- simulateData(model = population_parameters, sample.nobs = n)

config_data_group1$group <- "Group1"
config_data_group2$group <- "Group2"

config_data <- rbind(config_data_group1, config_data_group2)
#normality test for each item in both groups
shapiro_results <- list()

for (group_value in config_data$group) {
  cat("Group:", group_value, "\n")
  
  for (i in 1:4) {
    item_name <- paste0("item", i)
    item_data <- config_data[[item_name]][config_data$group == group_value]
    
    shapiro_result <- shapiro.test(item_data)
    
    shapiro_results[[paste(group_value, item_name, sep = "_")]] <- shapiro_result
  }
}
print(shapiro_results)

#visual inspection
par(mfrow = c(2, 2))

for (i in 1:4) {
  item_name <- paste0("item", i)
  
  hist(config_data[[item_name]][config_data$group == "Group1"],
       main = paste("Histogram of", item_name, "for Group1"),
       xlab = item_name,
       breaks = 12)
}

for (i in 1:4) {
  item_name <- paste0("item", i)
  hist(config_data[[item_name]][config_data$group == "Group2"],
       main = paste("Histogram of", item_name, "for Group2"),
       xlab = item_name,
       breaks = 12)
}

#change the layout to normal
#par(mfrow = c(1,1))

######
model <- "
    latent =~ item1 + item2 + item3 + item4"

config_data <- rbind(config_data_group1, config_data_group2)

cfa_config <- cfa(model, data = config_data , group = "group")

summary(cfa_config, fit.measures = TRUE, standardized = TRUE)


#####different loadings
#without having them on the likert scale 
#same as before
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

config_vs_metric <- compareFit(cfa_config, cfa_metric)
summary(config_vs_metric)

#### time spend for 1000 replications

set.seed(123)
n_replications <- 1000
n <- 100

group1_parameters <- "latent =~ item1 + item2 + item3 + item4"
group2_parameters <- "latent =~ item1 + item2 + item3 + 0.5*item4"

time <- numeric(n_replications)

for (i in 1:n_replications) {
  start_time <- Sys.time()
  
  # data for group 1
  metric_data_group1 <- simulateData(model = group1_parameters, sample.nobs = n)
  metric_data_group1$group <- "Group1"
  
  # data for group 2
  metric_data_group2 <- simulateData(model = group2_parameters, sample.nobs = n)
  metric_data_group2$group <- "Group2"
  
  metric_data <- rbind(metric_data_group1, metric_data_group2)
  
  model <- "
    latent =~ item1 + item2 + item3 + item4"
  
  metric_data <- rbind(metric_data_group1, metric_data_group2)
  
  cfa_metric <- cfa(model, data = metric_data , group = "group", group.equal = "loadings")
  
  end_time <- Sys.time()
  time[i] <- end_time - start_time
}

# Report the total time taken for all replications
total_time <- sum(time)
print(total_time)
# [1] 94.18005


##### different intercepts
set.seed(123)
n <- 100

intercepts_group1 <- c(1, 1, 1, 1)  # All intercepts set to 1 for Group 1
intercepts_group2 <- c(1, 1, 1, 0.5)  # Intercepts set to 1 for items 1, 2, 3 and 0.5 for item 4 for Group 2

model_group1 <- '
    latent =~ item1 + item2 + item3 + item4
'
model_group2 <- '
    latent =~ item1 + item2 + item3 + item4
'

intercept_data_group1 <- simulateData(model = model_group1, sample.nobs = n, ov.var = intercepts_group1)

intercept_data_group2 <- simulateData(model = model_group2, sample.nobs = n, ov.var = intercepts_group2)

intercept_data_group1$group <- "Group1"
intercept_data_group2$group <- "Group2"

intercept_data <- rbind(intercept_data_group1, intercept_data_group2)

model <- "
    latent =~ item1 + item2 + item3 + item4"
cfa_config <- cfa(model, data = intercept_data , group = "group")

summary(cfa_config, fit.measures = TRUE, standardized = TRUE)






