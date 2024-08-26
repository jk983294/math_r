data <- rpois(n = 100L, lambda = 5.)

# Define the statistic function
statistic_function <- function(data, indices) {
  resample <- data[indices]
  return(mean(resample))
}

# Perform the bootstrap
bootstrap_results <- boot::boot(data = data, statistic = statistic_function, R = 1000)
print(bootstrap_results)

# Extract the variance of estimator
variance_estimate <- var(bootstrap_results$t)
print(variance_estimate)

# Extract the confidence interval
confidence_interval <- boot::boot.ci(boot.out = bootstrap_results, type = c("norm", "basic", "perc", "bca"))
print(confidence_interval)
