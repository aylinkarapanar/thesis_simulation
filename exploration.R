# calculating the ratio of p-values for chi-square that are higher than 0.05 for each condition
satisfactory_chisq <- aggregate(ifelse(results$pvalue > 0.05, 1, 0), 
                                by = list(results$model_ratios, results$magnitude_level, results$group_size), 
                                FUN = mean)
names(satisfactory_chisq) <- c("model_ratios", "magnitude_level", "group_size", "satisfactory_chisq")

# calculating the percentage of RMSEA values that are satisfactory (< .05) according to Byrne (1994) for each condition
satisfactory_rmsea <- aggregate(interpret_rmsea(results$rmsea, rules = 'byrne1994'), 
                                           by = list(results$model_ratios, results$magnitude_level, results$group_size), 
                                           FUN = function(x) mean(x == "satisfactory"))

names(satisfactory_rmsea) <- c("model_ratios", "magnitude_level", "group_size", "satisfactory_rmsea")

# calculating the percentage of CFI values that are satisfactory (> .95) according to Hu & Bentler (1999) for each condition
satisfactory_cfi <- aggregate(interpret_cfi(results$cfi, rules = "hu&bentler1999"), 
                                         by = list(results$model_ratios, results$magnitude_level, results$group_size), 
                                         FUN = function(x) mean(x == "satisfactory"))

names(satisfactory_cfi) <- c("model_ratios", "magnitude_level", "group_size", "satisfactory_cfi")

# merge the ratios
ratios_table <- merge(satisfactory_chisq, satisfactory_rmsea)
ratios_table <- merge(ratios_table, satisfactory_cfi)

numeric_columns <- ratios_table[, !names(ratios_table) %in% c("model_ratios", "magnitude_level", "group_size")]

ratios_table[, !names(ratios_table) %in% c("model_ratios", "magnitude_level", "group_size")] <- round(numeric_columns, 3)

ratios_chisq_graph <- ggplot(ratios_table, aes(x=`group_size`, y= `satisfactory_chisq`,
                                              col=`magnitude_level`, group=`magnitude_level`)) +
  geom_point() + geom_line() + 
  facet_grid(. ~ `model_ratios`) +
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = " The Ratio of Satisfactory Chi-Square Values",
       subtitle = "Across Sample Group Size, Magnitude Level & Noninvariance Ratios",
       x = "Sample Size Per Group",
       y = "Ratio")

ratios_rmsea_graph <- ggplot(ratios_table, aes(x=`group_size`, y= `satisfactory_rmsea`,
                                               col=`magnitude_level`, group=`magnitude_level`)) +
  geom_point() + geom_line() + 
  facet_grid(. ~ `model_ratios`) + 
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = " The Ratio of Satisfactory RMSEA Values",
       subtitle = "Across Sample Group Size, Magnitude Level & Noninvariance Ratios",
       x = "Sample Size Per Group",
       y = "Ratio")

ratios_cfi_graph <- ggplot(ratios_table, aes(x=`group_size`, y= `satisfactory_cfi`,
                                             col=`magnitude_level`, group=`magnitude_level`)) +
  geom_point() + geom_line() + 
  facet_grid(. ~ `model_ratios`) + 
  scale_color_manual(name = "Magnitude Level", values = custom_colors) +
  labs(title = " The Ratio of Satisfactory CFI Values",
       subtitle = "Across Sample Group Size, Magnitude Level & Noninvariance Ratios",
       x = "Sample Size Per Group",
       y = "Ratio")

combined_ratio_plots <- grid.arrange(ratios_chisq_graph, ratios_rmsea_graph, ratios_cfi_graph, nrow = 3)
combined_file_path <- file.path(directory, "combined_ratio_plots.png")
ggsave(combined_file_path, plot = combined_ratio_plots, width = 12, height = 18)