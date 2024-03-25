# Define sample sizes and intercept values
n <- c(50, 100, 250, 500)
intercepts <- c(0.9, 0.8, 0.7, 0.6)

# Define the number of items
nitems <- 4

# Define a function to create the model string
create_model_string <- function(intercept, nitems) {
  item_names <- paste0("item", 1:nitems)
  pop.model <- paste0(item_names, collapse = "+" )
  pop.model <- paste('latent =~ ', pop.model)
  intercept_part <- c()
  
  # Loop to generate intercepts for each subset of items
  for (i in 1:length(values)) {
    output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
    
    # Add values stepwise to subsequent items
    output <- paste(output, "item1 ~1*", c(0, 0, 0, values[i]), "\n")
    output <- paste(output, "item2 ~1*", c(0, 0, values[i], values[i]), "\n")
    output <- paste(output, "item3 ~1*", c(0, values[i], values[i], values[i]), "\n")
    output <- paste(output, "item4 ~1*", c(values[i], values[i], values[i], values[i]), "\n")
  }
  
  
  pop.model <- paste(pop.model, "\n", paste(output, collapse = "\n"))
  return(pop.model)
}

# Define a dataframe to store results
results_df <- data.frame(
  sample_size = numeric(),
  magnitude_level = numeric(),
  chisq = numeric(),
  pvalue = numeric(),
  cfi = numeric(),
  rmsea = numeric()
)

# Loop through sample sizes, intercepts, and iterations
for (size in n) {
  for (intercept in intercepts) {
    # Define the model string
    model_string <- create_model_string(intercept = intercept, 
                                        nitems = nitems)
    
    # Print the model string to investigate
    print(model_string)
    
    for (iter in 1:iterations) {
      # Your remaining code for simulation, CFA, fit measures, and storing results goes here
      # ...
    }
  }
}


##################################
# Values to iterate over
values <- c(0.6, 0.7, 0.8, 0.9)

# Loop over values
for (i in 1:length(values)) {
  output <- paste("latent =~ item1 + item2 + item3 + item4", "\n")
  
  # Add values stepwise to subsequent items
  output <- paste(output, "item1 ~1*", c(0, 0, 0, values[i]), "\n")
  output <- paste(output, "item2 ~1*", c(0, 0, values[i], values[i]), "\n")
  output <- paste(output, "item3 ~1*", c(0, values[i], values[i], values[i]), "\n")
  output <- paste(output, "item4 ~1*", c(values[i], values[i], values[i], values[i]), "\n")
  
  cat(output, "\n")
}

#############################
# Define sample sizes and intercept values
n <- c(50, 100, 250, 500)
intercepts <- c(0.9, 0.8, 0.7, 0.6)

# Define the number of items
nitems <- 4

# Define a function to create the model string
create_model_string <- function(intercepts, nitems) {
  item_names <- paste0("item", 1:nitems)
  pop.model <- paste0(item_names, collapse = "+" )
  pop.model <- paste('latent =~ ', pop.model)
  intercept_part <- character(length(intercepts))
  
  for (i in seq_along(intercepts)) {
    output <- paste("latent =~", paste(item_names, collapse = " + "))
    for (j in 1:nitems) {
      intercept_values <- ifelse(1:nitems == j, intercepts[i], 0)
      output <- paste(output, paste(item_names[j], "~ 1 * ", intercept_values[j]))
    }
    intercept_part[i] <- output
  }
  
  return(intercept_part)
}

# Loop through sample sizes, intercepts, and iterations
for (size in n) {
  for (intercept in intercepts) {
    # Define the model strings
    model_strings <- create_model_string(intercepts = intercept, nitems = nitems)
    
    # Print the model strings to investigate
    for (model_string in model_strings) {
      print(model_string)
    }
    
    # Your remaining code for simulation, CFA, fit measures, and storing results goes here
    # ...
  }
}



