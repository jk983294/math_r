# Proximal Gradient Descent for Lasso Regression
# Objective: min_w [ (1/2n) * ||Xw - y||^2 + λ||w||_1 ]
lasso_pgd <- function(X, y, lambda, max_iter = 1000, tol = 1e-6) {
  n <- nrow(X)  # Number of samples
  p <- ncol(X)  # Number of features
  
  # Initialize coefficients to zero
  w <- rep(0, p)
  
  # Compute Lipschitz constant L (spectral norm of X'X)
  # Using largest singular value of X
  s <- svd(X, nu = 0, nv = 0)$d  # Singular values
  s_max <- max(s)
  L <- (s_max^2) / n              # Lipschitz constant for gradient of squared loss
  step_size <- 1 / L              # Step size (learning rate)
  
  # PGD iterations
  for (iter in 1:max_iter) {
    # Gradient of smooth part: ∇g(w) = (1/n) * X'(Xw - y)
    residual <- X %*% w - y
    grad <- (1/n) * crossprod(X, residual)  # Equivalent to (1/n) * X' * residual
    
    # Gradient step: z = w - t * ∇g(w)
    z <- w - step_size * grad
    
    # Proximal step (soft thresholding): prox_{tλ||·||₁}(z)
    gamma <- step_size * lambda  # Combined threshold
    w_new <- sign(z) * pmax(abs(z) - gamma, 0)
    
    # Check convergence (Euclidean norm of coefficient change)
    diff <- sqrt(sum((w_new - w)^2))
    if (diff < tol) break
    
    w <- w_new  # Update coefficients
  }
  
  # Return results
  list(
    coefficients = w,
    iterations = iter,
    converged = (iter < max_iter)
  )
}

# Helper function: Soft thresholding operator
soft_threshold <- function(z, gamma) {
  sign(z) * pmax(abs(z) - gamma, 0)
}

# PGD with Backtracking Line Search (Adaptive Step Size)
lasso_pgd_backtrack <- function(X, y, lambda, max_iter = 1000, tol = 1e-6, 
                               beta = 0.5, initial_step = 1.0, c = 0.5) {
  n <- nrow(X)
  p <- ncol(X)
  w <- rep(0, p)  # Initialize coefficients
  
  for (iter in 1:max_iter) {
    # Compute gradient of smooth part: ∇g(w) = (1/n) * X'(Xw - y)
    residual <- X %*% w - y
    grad <- (1/n) * crossprod(X, residual)
    
    # Backtracking line search
    t <- initial_step
    obj_old <- (1/(2*n)) * sum(residual^2)  # Current objective value
    
    for (bt in 1:20) {  # Max 20 backtracking steps
      # Candidate point after gradient step
      z_candidate <- w - t * grad
      
      # Compute objective at candidate
      residual_candidate <- X %*% z_candidate - y
      obj_candidate <- (1/(2*n)) * sum(residual_candidate^2)
      
      # Armijo condition (sufficient decrease)
      armijo_lhs <- obj_candidate
      armijo_rhs <- obj_old - c * t * sum(grad^2)
      
      if (armijo_lhs <= armijo_rhs) break  # Accept step size
      t <- beta * t  # Reduce step size
    }
    
    # Proximal step (soft thresholding) with accepted step size
    z <- w - t * grad
    w_new <- soft_threshold(z, t * lambda)
    
    # Check convergence
    diff <- sqrt(sum((w_new - w)^2))
    if (diff < tol) break
    
    w <- w_new  # Update coefficients
  }
  
  list(
    coefficients = w,
    iterations = iter,
    converged = (iter < max_iter),
    method = "PGD with Backtracking"
  )
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
    y <- x_curr + momentum * (x_curr - x_prev)  # Extrapolated point
    
    # Gradient at extrapolated point
    residual <- X %*% y - y
    grad <- (1/n) * crossprod(X, residual)
    
    # Gradient step
    z <- y - step_size * grad
    
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

# Run PGD Lasso
lambda <- 0.1  # Regularization strength
result <- lasso_pgd(X, y, lambda)
result_bt <- lasso_pgd_backtrack(X, y, lambda, max_iter = 1000)
result_fista <- lasso_fista(X, y, lambda, max_iter = 1000)

# View results
cat("Estimated coefficients:\n")
print(result$coefficients)
cat("\nConverged in", result$iterations, "iterations\n")
