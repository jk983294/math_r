lasso_subgradient <- function(A, b, lambda, max_iter = 10000, tol = 1e-5, alpha0 = 0.01) {
  n <- nrow(A)
  p <- ncol(A)
  x <- rep(0, p)  # Initialize as vector
  obj_history <- numeric(max_iter)
  
  for (k in 1:max_iter) {
    # Compute residual (ensure vector output)
    residual <- as.vector(A %*% x - b)
    
    # Objective value with safety check
    sq_res <- sum(residual^2)
    if (!is.finite(sq_res)) {
      warning("Non-finite residual at iteration ", k)
      break
    }
    abs_x <- abs(x)
    l1_norm <- sum(abs_x)
    if (!is.finite(l1_norm)) {
      warning("Non-finite L1 norm at iteration ", k)
      break
    }
    obj_val <- 0.5 * sq_res + lambda * l1_norm
    obj_history[k] <- obj_val
    
    # Gradient of smooth part (convert to vector)
    grad_smooth <- as.vector(t(A) %*% residual)
    
    # Subgradient of L1 norm (handle zeros properly)
    subgrad_l1 <- ifelse(x > 0, lambda, 
                        ifelse(x < 0, -lambda, 0))
    
    # Full subgradient (ensure vector)
    g <- grad_smooth + subgrad_l1
    g <- as.vector(g)
    
    # Step size (diminishing) with safety cap
    alpha_k <- min(alpha0 / sqrt(k), 1)  # Prevent excessively large steps
    
    # Update parameters (ensure vector output)
    x_new <- x - alpha_k * g
    x_new <- pmin(pmax(x_new, -1e10), 1e10)  # Clip extreme values
    
    # Check for non-finite values
    if (!all(is.finite(x_new))) {
      warning("Non-finite values encountered at iteration ", k)
      break
    }
    
    # Check convergence (using vector norms)
    diff_norm <- sqrt(sum((x_new - x)^2))
    
    if (diff_norm < tol) {
      x <- x_new
      obj_history <- obj_history[1:k]
      break
    }
    x <- x_new
  }
  
  # Trim obj_history if we broke early
  actual_iters <- min(k, max_iter)
  obj_history <- obj_history[1:actual_iters]
  
  list(
    x = x,                   # Solution vector
    obj_val = tail(obj_history, 1),  # Final objective value
    obj_history = obj_history,       # All objective values
    iter = actual_iters         # Iterations used
  )
}


set.seed(42)
n <- 100
p <- 20
A <- matrix(rnorm(n * p), nrow = n)
true_coef <- c(rep(1.5, 5), rep(0, p-5))
b <- as.vector(A %*% true_coef + rnorm(n, sd = 0.5))

# Run with safer parameters
lambda <- 0.1
step_size <- 0.01
result <- lasso_subgradient(A, b, lambda, alpha0 = step_size, max_iter = 5000)

# Check results
cat("Converged in", result$iter, "iterations\n")
cat("Objective value:", result$obj_val, "\n")
cat("Non-zero coefficients at indices:", which(abs(result$x) > 1e-3), "\n")

# Plot convergence
plot(result$obj_history, type = 'l', log = 'y',
     xlab = "Iteration", ylab = "Objective Value (log scale)",
     main = "Subgradient Method Convergence")
