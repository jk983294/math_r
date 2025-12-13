# =============================================
# 2D Gradient Descent with Backtracking Line Search
# Function: f(x, y) = (x - 1)^2 + (y - 2)^2 + 3
# Minimum at: x = 1, y = 2, f(x,y) = 3
# =============================================

# Define the function
f <- function(x, y) {
  return((x - 1)^2 + (y - 2)^2 + 3)
}

# Define the gradient of the function
grad_f <- function(x, y) {
  df_dx <- 2 * (x - 1)
  df_dy <- 2 * (y - 2)
  return(c(df_dx, df_dy))
}

# Backtracking Line Search
# Inputs:
#   x, y: current point
#   dx, dy: gradient components (∂f/∂x, ∂f/∂y)
#   alpha: initial step size (e.g., 1.0)
#   beta: reduction factor (e.g., 0.8)
#   c: sufficient decrease constant (e.g., 0.01)
# Returns: suitable step size alpha
backtrack <- function(x, y, dx, dy, alpha = 1.0, beta = 0.8, c = 0.01) {
  while (f(x - alpha * dx, y - alpha * dy) >
         f(x, y) - c * alpha * (dx^2 + dy^2)) {
    alpha <- beta * alpha
  }
  return(alpha)
}

# Gradient Descent with Backtracking Line Search
gradient_descent_2d_bl <- function(start_x, start_y, max_iter) {
  x <- start_x
  y <- start_y
  
  # For tracking
  x_hist <- numeric(max_iter)
  y_hist <- numeric(max_iter)
  f_hist <- numeric(max_iter)
  
  for (i in 1:max_iter) {
    # Compute gradient
    grad <- grad_f(x, y)
    dx <- grad[1]
    dy <- grad[2]
    
    # Compute step size using backtracking
    alpha <- backtrack(x, y, dx, dy, alpha = 1.0, beta = 0.8, c = 0.01)
    
    # Update position
    x <- x - alpha * dx
    y <- y - alpha * dy
    
    # Store history
    x_hist[i] <- x
    y_hist[i] <- y
    f_hist[i] <- f(x, y)
  }
  
  return(list(
    optimal_x = x,
    optimal_y = y,
    optimal_value = f(x, y),
    x_history = x_hist,
    y_history = y_hist,
    f_history = f_hist
  ))
}

# ===== Parameters =====
start_x <- 0
start_y <- 0
max_iter <- 30

# ===== Run Gradient Descent with Backtracking =====
result <- gradient_descent_2d_bl(start_x, start_y, max_iter)

# ===== Print Results =====
cat(sprintf("Optimized x: %.4f\n", result$optimal_x))
cat(sprintf("Optimized y: %.4f\n", result$optimal_y))
cat(sprintf("Minimum f(x, y): %.4f\n", result$optimal_value))

# ===== Plot Trajectory in 2D =====
plot(result$x_history, result$y_history, type = 'b', col = 'blue',
     xlab = 'x', ylab = 'y',
     main = '2D Gradient Descent with Backtracking Line Search',
     xlim = range(c(result$x_history, 1)),
     ylim = range(c(result$y_history, 2)))
points(1, 2, col = 'red', pch = 19, cex = 1.5)
text(1, 2, labels = "Minimum (1,2)", pos = 4, col = 'red')
legend("topright", legend = c("Descent Path", "True Minimum (1,2)"),
       col = c("blue", "red"), pch = c(1, 19))

# ===== Plot Function Value Over Iterations =====
plot(1:max_iter, result$f_history, type = 'b', col = 'darkgreen',
     xlab = 'Iteration', ylab = 'f(x, y)',
     main = 'Function Value vs Iteration (with Backtracking)')
abline(h = 3, col = 'red', lty = 2)
legend("topright", legend = c("f(x, y)", "Optimum f = 3"),
       col = c("darkgreen", "red"), lty = c(1, 2))
