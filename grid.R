n <- c(50, 100, 250, 500)
intercepts <- c(0.9, 0.8, 0.7, 0.6)
models <- c()

n <- c(50, 100, 250, 500)
intercepts <- c(0.9, 0.8, 0.7, 0.6)
models <- c()
magnitude_level <- c()
# Loop to generate model strings with stepwise increasing intercept values
for (i in 1:length(intercepts)) {
  output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
  # Add values stepwise to subsequent items
  output <- paste(output, "item1 ~1*", c(0, 0, 0, intercepts[i]), "\n")
  output <- paste(output, "item2 ~1*", c(0, 0, intercepts[i], intercepts[i]), "\n")
  output <- paste(output, "item3 ~1*", c(0, intercepts[i], intercepts[i], intercepts[i]), "\n")
  output <- paste(output, "item4 ~1*", c(intercepts[i], intercepts[i], intercepts[i], intercepts[i]), "\n")
  
  # Append the model string to the models vector
  models <- c(models, output)
  magnitude_level <- c(magnitude_level, intercepts[i])
}

# Create combinations of sample size, magnitude level, and model string
design <- expand.grid(model = models, sample_size = n)

# Add an index column
design$ndesign <- 1:nrow(design)
design$magnitude_level <- magnitude_level

