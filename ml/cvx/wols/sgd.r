# Set seed for reproducibility
set.seed(123)

# ------------------------------------------------------
# Part 1: Full demonstration with 2 predictors (no plot)
# ------------------------------------------------------

# Generate synthetic data
n <- 100  # number of observations
p <- 2    # number of predictors (excluding intercept)

# Predictors
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
# Add intercept column (column of 1s)
X_int <- cbind(1, X)

# True coefficients (including intercept)
beta_true <- c(2, 3, -1.5)

# Generate response with noise
y <- X_int %*% beta_true + rnorm(n, sd = 0.5)

# Define weights for each observation (randomly)
w <- runif(n, min = 0.1, max = 2.0)
# Emphasize some weights for effect
w[1:20] <- 0.1  # low weight
w[21:40] <- 2.0 # high weight

# --- Method 1: Closed-Form Solution ---
# Formula: beta_hat = (X'WX)^-1 * X'Wy
XtX_w <- t(X_int) %*% (w * X_int)  # X'WX
Xty_w <- t(X_int) %*% (w * y)      # X'Wy
beta_closed_form <- solve(XtX_w, Xty_w)

cat("--- Closed-Form Solution ---\n")
cat("Coefficients:", as.numeric(beta_closed_form), "\n\n")

# --- Method 2: Stochastic Gradient Descent (SGD) ---
# Initialize beta vector to zeros
beta_sgd <- matrix(0, nrow = p+1, ncol = 1)

# SGD parameters
eta <- 0.01       # learning rate (step size)
num_iterations <- 10000 # number of iterations

# Run SGD
for (k in 1:num_iterations) {
  # 1. Randomly select one observation
  i <- sample(1:n, 1)
  
  # 2. Get the data for that observation
  x_i <- as.numeric(X_int[i, ]) # Row as vector
  y_i <- y[i]
  w_i <- w[i]
  
  # 3. Calculate prediction and error
  pred <- sum(x_i * beta_sgd)
  err <- pred - y_i
  
  # 4. Compute gradient for the weighted loss: L = w_i*(x_i'*beta - y_i)^2
  # dL/dbeta = 2 * w_i * err * x_i
  grad <- 2 * w_i * err * x_i
  
  # 5. Update beta
  beta_sgd <- beta_sgd - eta * matrix(grad, ncol = 1)
}

cat("--- SGD Solution (after", num_iterations, "iterations) ---\n")
cat("Coefficients:", as.numeric(beta_sgd), "\n\n")

# --- Comparison ---
cat("--- Comparison ---\n")
cat("True Coefficients:     ", beta_true, "\n")
cat("Closed-Form Estimate:  ", round(as.numeric(beta_closed_form), 4), "\n")
cat("SGD Estimate (approx): ", round(as.numeric(beta_sgd), 4), "\n")

# ------------------------------------------------------
# Part 2: Simplified 1D case with visualization
# ------------------------------------------------------
par(mfrow = c(1, 1)) # Reset graphics layout

# Generate simple 1D data
n_simple <- 50
x_simple <- seq(-2, 2, length.out = n_simple)
X_simple <- cbind(1, x_simple) # Add intercept
y_simple <- 2 + 3 * x_simple + rnorm(n_simple, sd = 0.5)

# Define weights (e.g., higher weight for points near x=0)
w_simple <- exp(-x_simple^2) # Gaussian-like weights

# Closed-form for 1D
XtX_w_simple <- t(X_simple) %*% (w_simple * X_simple)
Xty_w_simple <- t(X_simple) %*% (w_simple * y_simple)
beta_simple <- solve(XtX_w_simple, Xty_w_simple)
intercept <- beta_simple[1]
slope <- beta_simple[2]

# Plotting
plot(x_simple, y_simple, pch = 16, col = rgb(0, 0, 0, 0.3), 
     main = "Weighted OLS Demo (1D)", xlab = "Predictor (x)", ylab = "Response (y)")
# Make point sizes proportional to weights
symbols_size <- (w_simple - min(w_simple)) / (max(w_simple) - min(w_simple)) * 5 + 1
points(x_simple, y_simple, cex = symbols_size, pch = 16, col = rgb(0, 0, 0, 0.3))

# Draw the weighted regression line
abline(intercept, slope, col = "red", lwd = 2)
legend("topleft", legend = c("Weighted Regression Line"), col = "red", lty = 1, lwd = 2)