#install.packages('lavaan')
#install.packages('effectsize')
library(lavaan)
library(effectsize)

set.seed(123)

n <- c(50, 100, 250, 500)
intercepts <- c(0.1, 0.2, 0.3, 0.4)
iterations <- 1:100
ratios <- c(0.25, 0.5, 0.75)
models <- c()


for (i in 1:length(intercepts)) {
  output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
  output <- paste(output, "item1 ~", c(0, 0, 0), "*1","\n")
  output <- paste(output, "item2 ~", c(0, 0, intercepts[i]), "*1", "\n")
  output <- paste(output, "item3 ~", c(0, intercepts[i], intercepts[i]),"*1", "\n")
  output <- paste(output, "item4 ~", c(intercepts[i], intercepts[i], intercepts[i]),"*1", "\n")
  
  models <- c(models, output)
}

design <- expand.grid(model = models, group_size = n, iteration = iterations)

design$ndesign <- 1:nrow(design)


results <- expand.grid(ndesign = design$ndesign,
                       chisq = 0,
                       pvalue = 0,
                       cfi = 0,
                       rmsea = 0)
model_ratios <- rep(ratios, times = length(intercepts)*length(iterations)*length(n))
results$model_ratios <- model_ratios

magnitude_level <- rep(intercepts, each = length(ratios), times = length(n)*length(iterations))
results$magnitude_level <- magnitude_level

results$group_size <- design$group_size

group1_string <- "latent =~ item1 + item2 + item3 + item4"

for (i in 1:nrow(design)){
  
  scalar_data_group1 <- simulateData(model = group1_string, sample.nobs = design$group_size[i])
  scalar_data_group2 <- simulateData(model = design$model[i], sample.nobs = design$group_size[i])

  scalar_data_group1$group <- "Group1"
  scalar_data_group2$group <- "Group2"
  
  scalar_data <- rbind(scalar_data_group1, scalar_data_group2)
  
  cfa_model <- cfa(model_string, data = scalar_data, group = "group", group.equal = c("loadings", "intercepts"))
  fit_measures <- fitMeasures(cfa_model, c("chisq", "pvalue", "cfi", "rmsea"))
 
  results$chisq[results$ndesign == i] <- fit_measures['chisq']
  results$pvalue[results$ndesign == i] <- fit_measures['pvalue']
  results$cfi[results$ndesign == i] <- fit_measures['cfi']
  results$rmsea[results$ndesign == i] <- fit_measures['rmsea']
}

results$group_size <- factor(results$group_size, levels = c("50", "100", "250", "500"))
results$magnitude_level <- factor(results$magnitude_level, levels = c("0.1", "0.2", "0.3", "0.4"))
results$model_ratios <- factor(results$model_ratios, levels = c("0.25", "0.5", "0.75", "1"))

anova_chisq <- aov(chisq ~ group_size * magnitude_level * model_ratios, data = results)
summary(anova_chisq)

anova_rmsea <- aov(rmsea ~ group_size * magnitude_level * model_ratios, data = results)
summary(anova_rmsea)

anova_cfi <- aov(cfi ~ group_size * magnitude_level * model_ratios, data = results)
summary(anova_cfi)

eta_chisq <- eta_squared(anova_chisq)
interpret_eta_squared(eta_chisq, rules = "cohen1992")

eta_rmsea <- eta_squared(anova_rmsea)
interpret_eta_squared(eta_rmsea, rules = "cohen1992")

eta_cfi <- eta_squared(anova_cfi)
interpret_eta_squared(eta_cfi, rules = "cohen1992")
