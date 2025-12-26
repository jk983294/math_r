# Weighted OLS via SAGA and SVRG
# Generate synthetic data
set.seed(42)
n <- 1000  # Number of samples
p <- 10    # Number of features
X <- cbind(1, matrix(rnorm(n * (p - 1)), ncol = p - 1))  # Design matrix with intercept
beta_true <- rnorm(p)  # True coefficients
y <- X %*% beta_true + rnorm(n, sd = 0.5)  # Response with noise
w <- runif(n, 0.5, 1.5)  # Sample weights
# Normalize features (critical for stability)
X[, -1] <- scale(X[, -1])

# Objective function (weighted OLS loss)
weighted_ols_loss <- function(beta, X, y, w) {
  mean_w <- mean(w)
  sum_w <- sum(w)
  0.5 * (sum(w * (y - X %*% beta)^2) / sum_w)
}

# Analytical solution (minimizer)
analytical_beta <- solve(t(X) %*% diag(w) %*% X, t(X) %*% (w * y))
analytical_loss <- weighted_ols_loss(analytical_beta, X, y, w)

# SAGA Algorithm
saga_ols <- function(X, y, w, max_iter = 1000, lr = 0.01) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)  # Initialize coefficients
  grad_table <- matrix(0, n, p)  # Store past gradients
  mean_grad <- rep(0, p)  # Mean of stored gradients
  
  # Initialize gradient table
  for (i in 1:n) {
    xi <- X[i, ]
    yi <- y[i]
    wi <- w[i]
    resid <- yi - sum(xi * beta)
    grad_table[i, ] <- -wi * resid * xi
  }
  mean_grad <- colMeans(grad_table)
  
  obj_history <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    i <- sample(n, 1)  # Random sample
    xi <- X[i, ]
    yi <- y[i]
    wi <- w[i]
    
    # Compute new gradient
    resid_new <- yi - sum(xi * beta)
    grad_new <- -wi * resid_new * xi
    
    # Update direction
    v <- grad_new - grad_table[i, ] + mean_grad
    beta <- beta - lr * v
    
    # Update mean gradient
    mean_grad <- mean_grad + (grad_new - grad_table[i, ]) / n
    grad_table[i, ] <- grad_new
    
    # Track objective
    obj_history[iter] <- weighted_ols_loss(beta, X, y, w)
  }
  
  list(beta = beta, obj_history = obj_history)
}

# SVRG Algorithm
svrg_ols <- function(X, y, w, max_iter = 1000, lr = 0.01, m = 100) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)  # Initialize coefficients
  obj_history <- numeric(max_iter)
  iter_count <- 0
  
  while (iter_count < max_iter) {
    beta_s <- beta  # Snapshot point
    grad_full <- rep(0, p)  # Full gradient storage
    
    # Compute full gradient at snapshot
    for (i in 1:n) {
      xi <- X[i, ]
      yi <- y[i]
      wi <- w[i]
      resid <- yi - sum(xi * beta_s)
      grad_full <- grad_full - wi * resid * xi
    }
    grad_full <- grad_full / n  # CRITICAL: Use AVERAGE
    
    # Inner loop
    inner_iters <- min(m, max_iter - iter_count)
    for (t in 1:inner_iters) {
      iter_count <- iter_count + 1
      i <- sample(n, 1)  # Random sample
      xi <- X[i, ]
      yi <- y[i]
      wi <- w[i]
      
      # Gradients at current beta and snapshot
      resid_beta <- yi - sum(xi * beta)
      grad_beta <- -wi * resid_beta * xi
      
      resid_s <- yi - sum(xi * beta_s)
      grad_s <- -wi * resid_s * xi
      
      # Variance-reduced update
      v <- grad_beta - grad_s + grad_full
      beta <- beta - lr * v
      
      # Track objective
      obj_history[iter_count] <- weighted_ols_loss(beta, X, y, w)
    }
  }
  
  list(beta = beta, obj_history = obj_history)
}

saga_result <- saga_ols(X, y, w, max_iter=20000, lr=0.003)
svrg_result <- svrg_ols(X, y, w, max_iter=20000, lr=0.005, m=10)

# Plot convergence
library(ggplot2)
df <- data.frame(
  Iteration = 1:1000,
  SAGA = saga_result$obj_history,
  SVRG = svrg_result$obj_history
)
df_long <- reshape2::melt(df, id.vars = "Iteration")

ggplot(df_long, aes(x = Iteration, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Convergence of Variance-Reduced SGD",
       subtitle = "Weighted OLS Loss",
       y = "Objective Value",
       x = "Iteration") +
  scale_y_log10() +
  theme_minimal()

cat("Analytical Loss:", analytical_loss, "\n")
cat("SAGA Final Loss:", tail(saga_result$obj_history, 1), "\n")
cat("SVRG Final Loss:", tail(svrg_result$obj_history, 1), "\n")
