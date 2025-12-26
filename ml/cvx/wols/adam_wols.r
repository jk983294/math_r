# Weighted OLS Regression with Adam Optimizer
# -------------------------------------------------
set.seed(42)

# 1. Generate synthetic data
n <- 200   # Samples
p <- 3     # Features (including intercept)

# Design matrix (add intercept column)
X <- cbind(1, matrix(rnorm(n*(p-1)), nrow=n))
true_beta <- c(2.5, -1.2, 0.7)  # True coefficients (intercept first)
weights <- runif(n, min=0.5, max=2.0)  # Sample weights

# Response variable with heteroscedastic noise
error <- rnorm(n, sd=1)
y <- X %*% true_beta + error

# 2. Analytical solution (for comparison)
weighted_X <- sqrt(weights) * X
weighted_y <- sqrt(weights) * y
analytical_solution <- solve(t(weighted_X) %*% weighted_X) %*% t(weighted_X) %*% weighted_y

# 3. Adam optimizer implementation
adam_weighted_ols <- function(X, y, weights, 
                              lr = 0.01,        # Learning rate
                              beta1 = 0.9,      # Momentum decay
                              beta2 = 0.999,    # Squared gradient decay
                              eps = 1e-8,       # Small constant
                              max_iter = 5000,  # Iterations
                              tol = 1e-6) {     # Convergence tolerance
    
    n <- nrow(X)
    p <- ncol(X)
    
    # Initialize parameters
    beta <- matrix(0, nrow=p, ncol=1)
    m <- matrix(0, nrow=p, ncol=1)  # First moment (momentum)
    v <- matrix(0, nrow=p, ncol=1)  # Second moment (squared gradients)
    loss_hist <- numeric(max_iter)     # Track loss history
    beta_hist <- array(0, dim = c(p, max_iter))  # Track parameters
    
    # Adam optimization loop
    for (t in 1:max_iter) {
        # Calculate predictions and residuals
        pred <- X %*% beta
        resid <- y - pred
        
        # Loss calculation (weighted SSE)
        loss <- sum(weights * resid^2)
        loss_hist[t] <- loss
        beta_hist[, t] <- beta

        # Gradient: dL/dβ = -2XᵀW(y - Xβ)
        grad <- -2 * t(X) %*% (weights * resid)
        
        # Update biased first moment estimate
        m <- beta1 * m + (1 - beta1) * grad
        
        # Update biased second raw moment estimate
        v <- beta2 * v + (1 - beta2) * (grad * grad)
        
        # Compute bias-corrected estimates
        m_hat <- m / (1 - beta1^t)
        v_hat <- v / (1 - beta2^t)
        
        # Update parameters
        beta_prev <- beta
        beta <- beta - lr * m_hat / (sqrt(v_hat) + eps)
        
        # Check convergence
        if (max(abs(beta - beta_prev)) < tol) {
            cat("Converged at iteration:", t, "\n")
            loss_hist <- loss_hist[1:t]
            beta_hist <- beta_hist[, 1:t, drop = FALSE]
            break
        }
    }
    list(beta = beta, loss_hist = loss_hist, beta_hist = beta_hist)
}

# 4. Run Adam optimization
adam_result <- adam_weighted_ols(
    X = X,
    y = y,
    weights = weights,
    lr = 0.05,        # Tuned learning rate
    max_iter = 10000
)

# 5. Compare results
cat("\nTrue coefficients:    ", true_beta, "\n")
cat("Analytical solution:  ", as.vector(analytical_solution), "\n")
cat("Adam solution:        ", as.vector(adam_result$beta), "\n\n")

# 6. Visualization
# Plot 1: Loss convergence
plot(adam_result$loss_hist, type = "l", col = "blue",
     xlab = "Iteration", ylab = "Loss (Weighted SSE)",
     main = "Loss Convergence", log = "y")
abline(h = sum(weights * (y - X %*% analytical_solution)^2), 
       col = "red", lty = 2, lwd = 2)

# Plot 2: Parameter trajectories
matplot(t(adam_result$beta_hist), type = "l", lty = 1,
        xlab = "Iteration", ylab = "Parameter Value",
        main = "Parameter Convergence")
abline(h = true_beta, col = 1:length(true_beta), lty = 2)
legend("topright", legend = paste0("β", 0:(p-1)), 
       col = 1:p, lty = 1, bty = "n")

# Plot 3: Final coefficients comparison
coef_df <- data.frame(
    Parameter = factor(paste0("β", 0:(p-1))),
    True = true_beta,
    Analytical = as.vector(analytical_solution),
    Adam = as.vector(adam_result$beta)
)

library(reshape2)
library(ggplot2)
coef_melt <- melt(coef_df, id.vars = "Parameter")
ggplot(coef_melt, aes(x = Parameter, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Coefficient Comparison", y = "Value") +
    theme_minimal()

# Plot 4: Gradient norms (last 100 iterations)
grad_norms <- sapply(1:nrow(adam_result$beta_hist), function(i) {
    if(i == 1) return(NA)
    beta_prev <- adam_result$beta_hist[, i-1]
    pred <- X %*% beta_prev
    resid <- y - pred
    grad <- -2 * t(X) %*% (weights * resid)
    sqrt(sum(grad^2))
})
plot(tail(grad_norms, 100), type = "l", 
     xlab = "Iteration (last 100)", ylab = "Gradient Norm",
     main = "Final Gradient Behavior")
abline(h = 0, col = "red", lty = 2)
