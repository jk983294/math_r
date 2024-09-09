n <- 10000L # number of observation
p <- 10L # number of predictor
odd_cols <- seq(1, 10, 2)
event_cols <- seq(2, 10, 2)

X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
X[, odd_cols] <- apply(X[, odd_cols], 2, function(x) 2 * x + 3)
X[, event_cols] <- apply(X[, odd_cols], 2, function(x) 3 * x + 2)
y <- apply(X[, 1:p], 1, sum) + rnorm(n)


# Perform PCA
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)

# Extract the principal components
PCs <- pca_result$x

# Select the first k principal components
k <- 5
selected_PCs <- PCs[, 1:5]

# Perform regression
regression_model <- lm(y ~ selected_PCs)

# Summary of the regression model
summary(regression_model)

# Get the loadings (rotation matrix)
loadings <- pca_result$rotation[, 1:k]

# Reconstruct the coefficients
beta_PC <- coefficients(regression_model)[-1]  # Exclude the intercept
beta_original <- loadings %*% beta_PC

# Add the intercept
beta_original <- c(coefficients(regression_model)[1], beta_original)

# Print the reconstructed coefficients
print(beta_original)
