set.seed(42)

# Parameters
d <- 1000          # dimension
n <- 10000         # number of samples

# Generate n samples from N(0, I_d)
X <- matrix(rnorm(n * d), nrow = n, ncol = d)

# Compute Euclidean norms
norms <- sqrt(rowSums(X^2))

# Normalize by sqrt(dimension)
normalized_norms <- norms / sqrt(d)

# Plot histogram
hist(
  normalized_norms,
  breaks = 50,
  col = "skyblue",
  border = "white",
  main = paste("Thin Shell Effect (d =", d, ")"),
  xlab = expression(paste("||X|| / ", sqrt(d))),
  ylab = "Density"
)

# Add vertical line at expected value
abline(v = 1, col = "red", lwd = 2, lty = 2)

# Summary statistics
cat("Mean:", mean(normalized_norms), "\n")
cat("SD:", sd(normalized_norms), "\n")
