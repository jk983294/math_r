library(dplyr)

# Function to perform sequential bootstrap
sequential_bootstrap <- function(data, block_length, n_bootstrap) {
  n <- length(data)  # Length of the time series
  n_blocks <- ceiling(n / block_length)  # Number of blocks
  bootstrap_samples <- list()  # List to store bootstrap samples

  for (i in 1:n_bootstrap) {
    # Step 1: Randomly sample blocks with replacement
    sampled_blocks <- sample(1:n_blocks, n_blocks, replace = TRUE)
    
    # Step 2: Construct the bootstrap sample by concatenating the sampled blocks
    bootstrap_sample <- unlist(lapply(sampled_blocks, function(block) {
      start <- (block - 1) * block_length + 1
      end <- min(block * block_length, n)
      data[start:end]
    }))
    
    # Store the bootstrap sample
    bootstrap_samples[[i]] <- bootstrap_sample
  }

  return(bootstrap_samples)
}

# Simulate a time series (e.g., stock returns)
time_series <- rnorm(100, mean = 0, sd = 1)  # 100 observations

# Parameters
block_length <- 5  # Length of each block
n_bootstrap <- 100  # Number of bootstrap samples

# Perform sequential bootstrap
bootstrap_samples <- sequential_bootstrap(time_series, block_length, n_bootstrap)

# Inspect the first bootstrap sample
head(bootstrap_samples[[1]])

acf(time_series)
