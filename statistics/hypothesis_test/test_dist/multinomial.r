# Observed frequencies
observed <- c(8, 12, 10, 14, 9, 7)

# Total number of trials
n <- sum(observed)

# Number of categories (faces of the die)
k <- length(observed)

# Expected probabilities under the null hypothesis (fair die)
expected_probs <- rep(1 / k, k)

observed_probs <- observed / n

# Chi-squared test statistic
chi_squared_stat <- n * sum((observed_probs - expected_probs)^2 / expected_probs)

# Degrees of freedom
df <- k - 1

# p-value
p_value <- 1 - pchisq(chi_squared_stat, df)

# Print results
cat("Chi-squared test statistic:", chi_squared_stat, "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", p_value, "\n")

# Alternatively, you can use the built-in chisq.test function
chisq_test_result <- chisq.test(observed, p = expected_probs)
print(chisq_test_result)
