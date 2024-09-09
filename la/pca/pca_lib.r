n <- 10000L # number of observation
p <- 10L # number of predictor
odd_cols <- seq(1, 10, 2)
event_cols <- seq(2, 10, 2)

X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
X[, odd_cols] <- apply(X[, odd_cols], 2, function(x) 2 * x + 3)
X[, event_cols] <- apply(X[, odd_cols], 2, function(x) 3 * x + 2)

pca_result <- prcomp(X, scale = TRUE)

summary(pca_result)

# Extract the loadings (rotation matrix)
loadings <- pca_result$rotation

# Extract the principal component scores, projected_data
scores <- pca_result$x # scores = X %*% loadings

# Extract eigenvalues (variances)
eigenvalues <- pca_result$sdev^2

# Scree plot
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Principal Component", ylab = "Eigenvalue")

# Plot the first two principal components
plot(scores[, 1], scores[, 2], xlab = "Principal Component 1", ylab = "Principal Component 2", pch = 16)

biplot(pca_result)
