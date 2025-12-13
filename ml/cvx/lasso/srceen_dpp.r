library(Matrix)
library(matrixcalc)

# DPP Screening Function
dpp_screen <- function(A, b, lambda, x, lambda_prev = NULL) {
  n <- nrow(A)
  p <- ncol(A)
  
  # Calculate residual: r = b - A*x
  r <- b - as.vector(A %*% x)
  
  # Compute correlations: c_j = |A_j^T r|
  c <- abs(crossprod(A, r))
  
  # Identify active set (non-zero coefficients)
  active <- which(abs(x) > 1e-6)
  
  # Initialize upper bounds with naive estimate
  u_bound <- c
  
  # Compute tighter bounds if active set exists
  if (length(active) > 0) {
    A_active <- A[, active, drop = FALSE]
    G_SS <- crossprod(A_active)  # Gram matrix of active set
    
    # Add small diagonal perturbation for stability
    G_SS <- G_SS + diag(1e-8, ncol(G_SS))
    
    # Cholesky decomposition: G_SS = L*L^T
    L <- chol(G_SS)
    
    # Solve L*v = A^T r for active set
    v_active <- solve(L, crossprod(A_active, r))
    w_active <- solve(t(L), v_active)
    
    # Compute projections for inactive features
    if (length(active) < p) {
      A_inactive <- A[, -active, drop = FALSE]
      for (j in seq_len(ncol(A_inactive))) {
        a_j <- A_inactive[, j]
        # Compute projection: (a_j^T w_active) / ||a_j_A||^2
        num <- crossprod(a_j, w_active)
        denom <- crossprod(a_j, solve(G_SS, a_j))
        
        if (denom > 1e-8) {
          proj <- num / denom
          # Update upper bound
          u_bound[-active][j] <- c[-active][j] + abs(proj - c[active])
        }
      }
    }
  }
  
  # Apply screening rule
  screened <- which(u_bound <= lambda)
  kept <- which(u_bound > lambda)
  
  list(
    screened = screened,      # Features that can be discarded
    kept = kept,              # Features that must be retained
    bounds = u_bound,         # Upper bounds |A_j^T r|
    correlations = c          # Raw correlations
  )
}

set.seed(123)
n <- 100
p <- 50
A <- matrix(rnorm(n * p), n, p)
b <- rnorm(n)

# Initial solution (all zeros)
x_init <- rep(0, p)
lambda <- 0.5

# Perform DPP screening
result <- dpp_screen(A, b, lambda, x_init)
print(paste("Screened features:", length(result$screened)))
print(paste("Retained features:", length(result$kept)))

# With non-zero initial solution
x_nonzero <- rnorm(p) * 0.1
result2 <- dpp_screen(A, b, lambda, x_nonzero)

# Sequential screening (when decreasing lambda)
lambda_new <- 0.8
result_seq <- dpp_screen(A, b, lambda_new, x_nonzero, lambda_prev = lambda)
result_seq$screened
