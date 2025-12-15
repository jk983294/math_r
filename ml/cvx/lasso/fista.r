# Accelerated Proximal Gradient Descent for Lasso Regression
# Objective: min_w [ (1/2n) * ||Xw - y||^2 + λ||w||_1 ]

# Helper function: Soft thresholding operator
soft_threshold <- function(z, gamma) {
  sign(z) * pmax(abs(z) - gamma, 0)
}

# FISTA (Accelerated PGD)
lasso_fista <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Compute Lipschitz constant (corrected)
  s <- svd(X, nu = 0, nv = 0)$d
  L <- max(s)^2 / n  # Correct Lipschitz constant
  step_size <- 1 / L  # Step size = 1/L
  
  # Initialize sequences
  x_prev <- rep(0, p)  # x_{k-1}
  x_curr <- rep(0, p)  # x_k
  t_prev <- 1          # t_{k-1}
  
  for (iter in 1:max_iter) {
    # Compute momentum term
    t_curr <- (1 + sqrt(1 + 4 * t_prev^2)) / 2
    momentum <- (t_prev - 1) / t_curr
    
    # Extrapolated point
    y_k <- x_curr + momentum * (x_curr - x_prev)  # y_k is the extrapolated point
    
    # Gradient at extrapolated point
    residual <- X %*% y_k - y  # y is response vector, y_k is coefficients
    grad <- (1/n) * crossprod(X, residual)
    
    # Gradient step
    z <- y_k - step_size * grad
    
    # Proximal step (soft thresholding)
    x_new <- soft_threshold(z, step_size * lambda)
    
    # Check convergence
    diff <- sqrt(sum((x_new - x_curr)^2))
    if (diff < tol) break
    
    # Update sequences
    x_prev <- x_curr
    x_curr <- x_new
    t_prev <- t_curr
  }
  
  list(
    coefficients = x_curr,
    iterations = iter,
    converged = (iter < max_iter),
    method = "FISTA"
  )
}

set.seed(123)
n <- 100  # Samples
p <- 10   # Features
X <- matrix(rnorm(n * p), nrow = n)
true_coef <- c(rep(1.5, 3), rep(0, p - 3))  # Only first 3 features matter
y <- X %*% true_coef + rnorm(n, sd = 0.5)

# Run FISTA Lasso
lambda <- 0.1  # Regularization strength
result_fista <- lasso_fista(X, y, lambda, max_iter = 1000)

# View results
cat("Estimated coefficients:\n")
print(result_fista$coefficients)
cat("\nConverged in", result_fista$iterations, "iterations\n")
