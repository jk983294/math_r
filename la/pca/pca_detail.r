# Step 1: Standardize the Data
standardize <- function(X) {
    X_mean <- colMeans(X)
    X_sd <- apply(X, 2, sd)
    X_standardized <- sweep(sweep(X, 2, X_mean, `-`), 2, X_sd, `/`)
    return(X_standardized)
}

# Step 2: Compute the Covariance Matrix
compute_covariance_matrix <- function(X_standardized) {
    n <- nrow(X_standardized)
    cov_matrix <- (1 / (n - 1)) * t(X_standardized) %*% X_standardized
    return(cov_matrix)
}

# Step 3: Eigen Decomposition
perform_eigen_decomposition <- function(cov_matrix) {
    eigen_decomp <- eigen(cov_matrix)
    eigenvalues <- eigen_decomp$values
    eigenvectors <- eigen_decomp$vectors
    return(list(eigenvalues = eigenvalues, eigenvectors = eigenvectors))
}

# Step 4: Sort Eigenvalues and Eigenvectors
sort_eigenvalues_and_vectors <- function(eigenvalues, eigenvectors) {
    sorted_indices <- order(eigenvalues, decreasing = TRUE) # sort return order
    sorted_eigenvalues <- eigenvalues[sorted_indices]
    sorted_eigenvectors <- eigenvectors[, sorted_indices]
    return(list(eigenvalues = sorted_eigenvalues, eigenvectors = sorted_eigenvectors))
}

# Step 5: Select Principal Components
select_principal_components <- function(sorted_eigenvectors, k) {
    principal_components <- sorted_eigenvectors[, 1:k]
    return(principal_components)
}

# Step 6: Project Data onto Principal Components
project_data <- function(X_standardized, principal_components) {
    projected_data <- X_standardized %*% principal_components
    return(projected_data)
}

n <- 10000L # number of observation
p <- 10L # number of predictor
odd_cols <- seq(1, 10, 2)
event_cols <- seq(2, 10, 2)

X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
X[, odd_cols] <- apply(X[, odd_cols], 2, function(x) 2 * x + 3)
X[, event_cols] <- apply(X[, odd_cols], 2, function(x) 3 * x + 2)

# Standardize the data
X_standardized <- standardize(X)

# Compute the covariance matrix
cov_matrix <- compute_covariance_matrix(X_standardized)

# Perform eigen decomposition
eigen_decomp <- perform_eigen_decomposition(cov_matrix)

# Sort eigenvalues and eigenvectors
sorted_decomp <- sort_eigenvalues_and_vectors(eigen_decomp$eigenvalues, eigen_decomp$eigenvectors)

# Select the top 2 principal components
k <- 2
principal_components <- select_principal_components(sorted_decomp$eigenvectors, k)

# Project the data onto the principal components
projected_data <- project_data(X_standardized, principal_components)

# View the reduced-dimensional data
plot(projected_data[, 1], projected_data[, 2])
