n <- 100L  # Sample size
lambda_true <- 0.5  # True lambda
sample <- rexp(n, rate = lambda_true)

# Maximum Likelihood Estimator (MLE) of lambda
lambda_hat <- 1 / mean(sample)

# Delta method to approximate the variance of the estimator
# The variance of the sample mean is sigma^2 / n, where sigma^2 is the variance of the exponential distribution
# For an exponential distribution, sigma^2 = 1 / lambda^2
# Therefore, the variance of the sample mean is (1 / lambda^2) / n
# The MLE of lambda is 1 / sample_mean, so the variance of lambda_hat is (1 / sample_mean^2) * (1 / n)

variance_lambda_hat <- (1 / mean(sample)^2) / n

# Standard error of lambda_hat
se_lambda_hat <- sqrt(variance_lambda_hat)

# Print the results
cat("Estimated lambda (MLE):", lambda_hat, "\n")
cat("Standard error of lambda_hat:", se_lambda_hat, "\n")

# Confidence interval for lambda
alpha <- 0.05  # Significance level
z <- qnorm(1 - alpha / 2)  # Z-score for 95% confidence interval
ci_lower <- lambda_hat - z * se_lambda_hat
ci_upper <- lambda_hat + z * se_lambda_hat

cat("95% Confidence Interval for lambda: [", ci_lower, ",", ci_upper, "]\n")
