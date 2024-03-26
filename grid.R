#install.packages('lavaan')
library(lavaan)


n <- c(50, 100, 250, 500)
intercepts <- c(0.9, 0.8, 0.7, 0.6)
models <- c()


#for (i in 1:length(intercepts)) {
  #output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
  #output <- paste(output, "item1 ~", c(0, 0, 0, intercepts[i]), "*1","\n")
  #output <- paste(output, "item2 ~", c(0, 0, intercepts[i], intercepts[i]), "*1", "\n")
  #output <- paste(output, "item3 ~", c(0, intercepts[i], intercepts[i], intercepts[i]),"*1", "\n")
  #output <- paste(output, "item4 ~", c(intercepts[i], intercepts[i], intercepts[i], intercepts[i]),"*1", "\n")
  
  #models <- c(models, output)
#}

#for (intercept in intercepts) {
#  output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
#  for (i in 1:length(intercepts)) {
#    intercept_vector <- c(rep(0, i - 1), intercept, rep(intercept, 4 - i))
#    output <- paste(output, paste("item", i, "~", intercept_vector, "*1", "\n"))
#  }
#  models <- c(models, output)
#}

for (i in 1:length(intercepts)) {
  intercept <- intercepts[i]
  output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
  intercept_vector <- c(rep(0, i - 1), intercept, rep(intercept, 4 - i))
  
  for (j in 1:length(intercepts)) {
    output <- paste(output, paste("item", j, "~", intercept_vector[j], "*1", "\n"))
  }
  
  models[i] <- output
}


design <- expand.grid(model = models, sample_size = n)


magnitude_level <- rep(intercepts, each = 4, times = 4)
design$magnitude_level <- magnitude_level
design$ndesign <- 1:nrow(design)

#where to put set seed?
set.seed(123)

results <- expand.grid(ndesign = design$ndesign,
                      ratio = 0,
                      magnitude_level = 0,
                      chisq = 0,
                      pvalue = 0,
                      cfi = 0,
                      rmsea = 0)

model_string <- "latent =~ item1 + item2 + item3 + item4"

for (i in 1:nrow(design)){
  
  scalar_data_group1 <- simulateData(model = model_string, sample.nobs = design$sample_size[i])
  scalar_data_group2 <- simulateData(model = design$model[i], sample.nobs = design$sample_size[i])

  scalar_data_group1$group <- "Group1"
  scalar_data_group2$group <- "Group2"
  
  scalar_data <- rbind(scalar_data_group1, scalar_data_group2)
  
  cfa_metric <- cfa(model_string, data = scalar_data , group = "group", group.equal = "loadings")
  print(design$magnitude_level[i])
  print(summary(cfa_metric)) 
}



