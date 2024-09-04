sample_data <- rnorm(50, mean = 0, sd = 1)

plot_cdf <- function(data) {
    ecdf_function <- ecdf(data)
    plot(ecdf_function,
        main = "Empirical Cumulative Distribution Function",
        xlab = "Sample Data", ylab = "Cumulative Probability", col = "blue"
    )
    curve(pnorm(x, mean = mean(data), sd = sd(data)), add = TRUE, col = "red", lwd = 2)
    legend("bottomright", legend = c("ECDF", "CDF"), col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")
}
plot_cdf(sample_data)

# Perform Kolmogorov-Smirnov test
# 1. Calculate the Empirical Cumulative Distribution Function (ECDF):
#   Arrange the sample data in ascending order.
#   Calculate the ECDF by plotting the proportion of data points that are less than or equal to each value in the sample.
# 2. Calculate the Theoretical Cumulative Distribution Function (CDF):
#   For the hypothesized distribution, calculate the CDF for each value in the sample.
# 3.Compute the KS Statistic:
#   The KS statistic (D) is the maximum absolute difference between the ECDF and the theoretical CDF.
ks_test_result <- ks.test(sample_data, "pnorm", mean = 0, sd = 1)

# Print the test result
print(ks_test_result)

# Interpret the result
alpha <- 0.05
if (ks_test_result$p.value < alpha) {
    cat("Reject the null hypothesis: The data does not follow a normal distribution.\n")
} else {
    cat("Fail to reject the null hypothesis: The data may follow a normal distribution.\n")
}
