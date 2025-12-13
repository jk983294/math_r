library(CVXR)
library(glmnet)

# Dantzig Selector implementation
# Mathematical formulation:
# minimize ||β||₁ 
# subject to: ||X'Xβ - X'y||∞ ≤ δ
# 
# Where:
# - β is the coefficient vector to be estimated
# - X is the design matrix (n × p)  
# - y is the response vector (n × 1)
# - δ is a tuning parameter controlling constraint tightness
# - ||·||₁ is the L1 norm (sum of absolute values)
# - ||·||∞ is the infinity norm (maximum absolute value)

dantzig_selector <- function(X, y, delta) {
  if (!is.matrix(X)) {
    stop("X must be a matrix")
  }
  if (!is.numeric(y) || length(y) != nrow(X)) {
    stop("y must be a numeric vector with length equal to nrow(X)")
  }
  if (delta <= 0) {
    stop("delta must be positive")
  }
  
  # Problem dimensions
  n <- nrow(X)
  p <- ncol(X)
  
  # Pre-compute matrix products for efficiency
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  
  # Define optimization variable (coefficient vector)
  beta <- Variable(p)
  
  # Define objective: minimize L1 norm of beta
  objective <- Minimize(norm1(beta))
  
  # Define constraint: infinity norm constraint on correlations
  # This ensures all absolute correlations are bounded by delta
  constraints <- list(norm_inf(XtX %*% beta - Xty) <= delta)
  
  problem <- Problem(objective, constraints)
  result <- solve(problem, solver = "ECOS")  # ECOS is efficient for this type of problem
  
  if (result$status != "optimal") {
    warning(paste("Solution status:", result$status))
  }
  
  return(result$getValue(beta))
}

# Cross-validation for delta parameter selection
dantzig_selector_cv <- function(X, y, delta_seq = NULL, nfolds = 5) {
  n <- nrow(X)
  
  # Default delta sequence if not provided
  if (is.null(delta_seq)) {
    # Create a log-spaced sequence of delta values
    # Scale based on the maximum correlation in the data
    max_corr <- max(abs(t(X) %*% y))
    delta_seq <- exp(seq(log(0.01 * max_corr), log(max_corr), length.out = 10))
  }
  
  # Create fold indices for cross-validation
  fold_indices <- sample(rep(1:nfolds, length.out = n))
  
  # Store CV errors for each delta
  cv_errors <- numeric(length(delta_seq))
  
  # Perform k-fold cross-validation
  for (fold in 1:nfolds) {
    # Split data into training and validation
    train_idx <- which(fold_indices != fold)
    val_idx <- which(fold_indices == fold)
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_val <- X[val_idx, , drop = FALSE]
    y_val <- y[val_idx]
    
    # Test each delta value
    for (i in seq_along(delta_seq)) {
      delta <- delta_seq[i]
      
      # Fit Dantzig Selector on training data
      beta_cv <- dantzig_selector(X_train, y_train, delta)
      
      # Predict on validation set
      y_pred <- X_val %*% beta_cv
      
      # Calculate mean squared error
      cv_errors[i] <- cv_errors[i] + mean((y_val - y_pred)^2)
    }
  }
  
  # Average errors across folds
  cv_errors <- cv_errors / nfolds
  
  # Select delta with minimum CV error
  best_idx <- which.min(cv_errors)
  best_delta <- delta_seq[best_idx]
  
  # Fit final model with best delta on all data
  beta_final <- dantzig_selector(X, y, best_delta)
  
  # Return results
  return(list(
    coefficients = beta_final,
    best_delta = best_delta,
    delta_sequence = delta_seq,
    cv_errors = cv_errors,
    cv_results = data.frame(delta = delta_seq, cv_error = cv_errors)
  ))
}

n <- 100L # number of observation
p <- 1000L # number of predictor
X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
y <- apply(X[, 1:p], 1, sum) + rnorm(n)

delta_value <- 0.1  # Tuning parameter - may need adjustment based on data scale
beta_hat <- dantzig_selector(X, y, delta = delta_value)

# Display results
cat("Dantzig Selector Results:\n")
cat("Number of non-zero coefficients:", sum(abs(beta_hat) > 1e-8), "\n")
cat("Total coefficients:", length(beta_hat), "\n")
cat("Sparsity ratio:", sum(abs(beta_hat) > 1e-8) / length(beta_hat), "\n")

# Show first 20 estimated coefficients
cat("\nFirst 20 estimated coefficients:\n")
print(round(beta_hat[1:20], 6))

# Compare with ordinary least squares (for reference, though not applicable when p > n)
# We'll use a subset of variables for comparison
p_subset <- min(50, p)  # Use first 50 variables or all if less
X_subset <- X[, 1:p_subset]
ols_solution <- solve(t(X_subset) %*% X_subset, t(X_subset) %*% y)

cat("\nComparison with OLS (using first", p_subset, "variables):\n")
cat("OLS coefficients (first 10):", round(ols_solution[1:min(10, length(ols_solution))], 6), "\n")
cat("DS coefficients (first 10):", round(beta_hat[1:min(10, length(beta_hat))], 6), "\n")

# Demonstrate cross-validation for delta selection
cat("\n" , rep("=", 50), "\n")
cat("CROSS-VALIDATION FOR DELTA SELECTION\n")
cat(rep("=", 50), "\n")

# Run cross-validation (using a smaller dataset for speed due to high dimensionality)
n_cv <- 50L  # Reduce observations for faster CV
p_cv <- 100L  # Reduce predictors for faster CV
X_cv <- matrix(rnorm(n_cv * p_cv, 0, 1), nrow = n_cv, ncol = p_cv)
y_cv <- apply(X_cv[, 1:p_cv], 1, sum) + rnorm(n_cv)

# Perform cross-validation
cv_results <- dantzig_selector_cv(X_cv, y_cv, nfolds = 3)  # 3-fold CV for speed

# Display CV results
cat("Best delta selected by cross-validation:", cv_results$best_delta, "\n")
cat("Number of non-zero coefficients with best delta:", 
    sum(abs(cv_results$coefficients) > 1e-8), "\n")

# Show CV error curve
cat("\nCross-validation results:\n")
print(cv_results$cv_results)

# Plot CV error curve (if plotting is available)
cat("\nNote: For better visualization, you can plot the CV error curve:\n")
cat("plot(cv_results$delta_sequence, cv_results$cv_errors, type='b',\n")
cat("     xlab='Delta', ylab='CV Error', main='Cross-Validation Error Curve')\n")

# Performance comparison with Lasso (using glmnet package)
cat("\n", rep("=", 50), "\n")
cat("PERFORMANCE COMPARISON WITH LASSO\n")
cat(rep("=", 50), "\n")

# Fit Lasso with cross-validation
lasso_cv <- cv.glmnet(X, y, alpha = 1)  # alpha=1 for Lasso

# Get Lasso coefficients at best lambda
lasso_beta <- as.numeric(coef(lasso_cv, s = "lambda.min"))[-1]  # Remove intercept

# Compare results
cat("Model Comparison:\n")
cat("Dantzig Selector:\n")
cat("  Non-zero coefficients:", sum(abs(beta_hat) > 1e-8), "/", length(beta_hat), "\n")
cat("  Sparsity ratio:", sum(abs(beta_hat) > 1e-8) / length(beta_hat), "\n")

cat("Lasso (CV-optimized):\n")
cat("  Non-zero coefficients:", sum(abs(lasso_beta) > 1e-8), "/", length(lasso_beta), "\n")
cat("  Sparsity ratio:", sum(abs(lasso_beta) > 1e-8) / length(lasso_beta), "\n")

# Prediction comparison on a test set
set.seed(123)  # For reproducible results
n_test <- 50
X_test <- matrix(rnorm(n_test * p, 0, 1), nrow = n_test, ncol = p)
y_test <- apply(X_test[, 1:p], 1, sum) + rnorm(n_test)

# Predictions
y_pred_ds <- X_test %*% beta_hat
y_pred_lasso <- X_test %*% lasso_beta

# Calculate MSE
mse_ds <- mean((y_test - y_pred_ds)^2)
mse_lasso <- mean((y_test - y_pred_lasso)^2)

cat("\nPrediction Performance (Test Set):\n")
cat("Dantzig Selector MSE:", mse_ds, "\n")
cat("Lasso MSE:", mse_lasso, "\n")
cat("Relative difference:", (mse_ds - mse_lasso) / mse_lasso * 100, "%\n")


# Summary and interpretation
cat("\n", rep("=", 50), "\n")
cat("SUMMARY AND INTERPRETATION\n")
cat(rep("=", 50), "\n")
cat("The Dantzig Selector is particularly useful for:\n")
cat("1. High-dimensional settings (p >> n)\n")
cat("2. When you want sparsity via L1 minimization\n")
cat("3. When correlation structure is important (infinity norm constraint)\n")
cat("4. As an alternative to Lasso with different theoretical properties\n")
cat("\nKey difference from Lasso:\n")
cat("- Lasso: minimize ||y - Xβ||² + λ||β||₁\n")
cat("- Dantzig: minimize ||β||₁ subject to ||X'(y - Xβ)||∞ ≤ δ\n")
cat("\nBoth methods promote sparsity but have different optimization landscapes.\n")