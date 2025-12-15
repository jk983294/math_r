# Projected Gradient Descent for Affine Constraints Ax = b
# Objective: min_x f(x) s.t. A x = b

# Projection onto affine set {x: A x = b}
proj_affine <- function(z, A, b) {
  # Convert inputs to matrices
  z <- matrix(z, ncol = 1)
  A <- as.matrix(A)
  b <- matrix(b, ncol = 1)
  
  # Compute projection: x = z - A^T (A A^T)^{-1} (A z - b)
  Az <- A %*% z
  rhs <- Az - b
  M <- A %*% t(A)
  
  # Solve M w = rhs
  if (det(M) < 1e-10) {
    warning("Matrix A*A' is near-singular. Using pseudoinverse.")
    w <- MASS::ginv(M) %*% rhs
  } else {
    w <- solve(M, rhs)
  }
  
  x <- z - t(A) %*% w
  as.vector(x)
}

# PGD optimizer for affine constraints
pgd_affine <- function(f, grad_f, A, b, x0, alpha, max_iter = 1000, tol = 1e-6) {
  x <- x0
  for (iter in 1:max_iter) {
    # Gradient step
    g <- grad_f(x)
    y <- x - alpha * g
    
    # Projection step
    x_new <- proj_affine(y, A, b)
    
    # Check convergence
    if (sqrt(sum((x_new - x)^2)) < tol) break
    x <- x_new
  }
  list(x = x_new, iter = iter, converged = (iter < max_iter))
}

# Projected Nesterov AGD Optimizer
projected_nesterov_agd <- function(f, grad_f, proj_C, A, b, x0, alpha, max_iter = 1000, tol = 1e-6) {
  # Initialize sequences
  x_prev <- x0  # x_{k-1}
  x_curr <- x0  # x_k
  y <- x0       # Extrapolated point y_k
  t <- 1.0      # Momentum parameter t_k
  
  for (iter in 1:max_iter) {
    # Gradient at extrapolated point y
    grad_y <- grad_f(y)
    
    # Gradient step + projection
    z <- y - alpha * grad_y
    x_next <- proj_C(z, A, b)  # Projection onto Ax=b
    
    # Check convergence
    if (sqrt(sum((x_next - x_curr)^2)) < tol) break
    
    # Update momentum parameter
    t_next <- (1 + sqrt(1 + 4 * t^2)) / 2
    beta <- (t - 1) / t_next
    # beta <- (k - 1) / (k + 2) # or smoothed weight
    
    # Extrapolate new y: y_{k+1} = x_{k+1} + β_k (x_{k+1} - x_k)
    y_next <- x_next + beta * (x_next - x_curr)
    
    # Update sequences
    x_prev <- x_curr
    x_curr <- x_next
    y <- y_next
    t <- t_next
  }
  
  list(
    x = x_curr,
    iterations = iter,
    converged = (iter < max_iter),
    f_val = f(x_curr)
  )
}

# Example: Quadratic minimization with linear constraint
# f(x) = 1/2 (x1^2 + 2x2^2) s.t. x1 + x2 = 1
# Solution should be (2/3, 1/3) ≈ (0.6667, 0.3333)

# Define objective function and gradient
f <- function(x) {
  0.5 * (x[1]^2 + 2 * x[2]^2)
}

grad_f <- function(x) {
  c(x[1], 2 * x[2])
}

# Affine constraint: x1 + x2 = 1
A <- matrix(c(1, 1), nrow = 1)  # Constraint matrix
b <- 1                           # Constraint value

# Parameters
x0 <- c(0, 0)                    # Initial guess
alpha <- 0.5                     # Step size (1/L, L=2 for Hessian diag(1,2))

# Run PGD
result <- pgd_affine(f, grad_f, A, b, x0, alpha)

# Print results
cat("Optimal solution:", round(result$x, 4), "\n")
cat("Iterations:", result$iter, "\n")
cat("Converged:", result$converged, "\n")
cat("Function value at solution:", f(result$x), "\n")

result_agd <- projected_nesterov_agd(f, grad_f, proj_affine, A, b, x0, alpha)
analytical_sol <- c(2/3, 1/3)  # (0.6667, 0.3333)

# Print results
cat("Nesterov AGD Solution: ", round(result_agd$x, 4), "\n")
cat("Iterations: ", result_agd$iterations, "\n")
cat("Error: ", sqrt(sum((result_agd$x - analytical_sol)^2)), "\n")
