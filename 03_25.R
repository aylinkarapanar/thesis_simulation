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
  for (i in 1:nitems) {
      intercept_part <- c(intercept_part, paste(item_names[(nitems - i + 1):nitems], "~ 1*", intercept))
  }
  
  pop.model <- paste(pop.model, "\n", paste(intercept_part, collapse = "\n"))
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
