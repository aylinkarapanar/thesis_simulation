# calculating the mean and standard deviation of chi-square for each condition
mean_chisq <- aggregate(results$chisq, by = list(results$model_ratios, results$magnitude_level, results$group_size), FUN = mean)
names(mean_chisq) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "Mean Chi-square")

sd_chisq <- aggregate(results$chisq, by = list(results$model_ratios, results$magnitude_level, results$group_size), FUN = sd)
names(sd_chisq) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "SD Chi-square")

# calculating the ratio of p-values for chi-square that are higher than 0.05 for each condition
satisfactory_chisq <- aggregate(ifelse(results$pvalue > 0.05, 1, 0), 
                                by = list(results$model_ratios, results$magnitude_level, results$group_size), 
                                FUN = mean)
names(satisfactory_chisq) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "Satisfactory Chi-square Ratio")

# calculating the mean and standard deviation of RMSEA for each condition 
mean_rmsea <- aggregate(results$rmsea, by = list(results$model_ratios, results$magnitude_level, results$group_size), FUN = mean)
names(mean_rmsea) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "Mean RMSEA")

sd_rmsea <- aggregate(results$rmsea, by = list(results$model_ratios, results$magnitude_level, results$group_size), FUN = sd)
names(sd_rmsea) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "SD RMSEA")

# calculating the percentage of RMSEA values that are satisfactory (< .05) according to Byrne (1994) for each condition
satisfactory_rmsea <- aggregate(interpret_rmsea(results$rmsea, rules = 'byrne1994'), 
                                           by = list(results$model_ratios, results$magnitude_level, results$group_size), 
                                           FUN = function(x) mean(x == "satisfactory"))

names(satisfactory_rmsea) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "Satisfactory RMSEA Ratio")


# calculating the mean and standard deviation of CFI for each condition 
mean_cfi <- aggregate(results$cfi, by = list(results$model_ratios, results$magnitude_level, results$group_size), FUN = mean)
names(mean_cfi) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "Mean CFI")

sd_cfi <- aggregate(results$cfi, by = list(results$model_ratios, results$magnitude_level, results$group_size), FUN = sd)
names(sd_cfi) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "SD CFI")

# calculating the percentage of RMSEA values that are satisfactory (> .95) according to Hu & Bentler (1999) for each condition
satisfactory_cfi <- aggregate(interpret_cfi(results$cfi, rules = "hu&bentler1999"), 
                                         by = list(results$model_ratios, results$magnitude_level, results$group_size), 
                                         FUN = function(x) mean(x == "satisfactory"))

names(satisfactory_cfi) <- c("Noninvariance Ratios", "Intercept Value", "Sample Group Size", "Satisfactory CFI Ratio")


# merge mean, standard deviation, and percentages
final_table <- merge(mean_chisq, sd_chisq)
final_table <- merge(final_table, satisfactory_chisq)
final_table <- merge(final_table, mean_rmsea)
final_table <- merge(final_table, sd_rmsea)
final_table <- merge(final_table, satisfactory_rmsea)
final_table <- merge(final_table, mean_cfi)
final_table <- merge(final_table, sd_cfi)
final_table <- merge(final_table, satisfactory_cfi)

numeric_columns <- final_table[, !names(final_table) %in% c("Noninvariance Ratios", "Intercept Value", "Sample Group Size")]
rounded_numeric <- round(numeric_columns, 3)

# Assign rounded values back to final_table
final_table[, !names(final_table) %in% c("Noninvariance Ratios", "Intercept Value", "Sample Group Size")] <- rounded_numeric

# Print final table
print(final_table)


