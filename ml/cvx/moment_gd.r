# =============================================
# 2D Gradient Descent with Momentum
# Function: f(x, y) = x^2 + b * y^2
# Minimum at: x = 0, y = 0, f(x,y) = 0
# Momentum accelerates descent along shallow (y) direction
# =============================================

# ===== Parameters =====
b <- 0.5          # anisotropy parameter: smaller b -> flatter in y
max_iter <- 50

# Hessian of f(x,y) = x^2 + b*y^2 is diag(2, 2*b):
# eigenvalues {2, 2*b}, so lambda_max = 2*max(1,b), lambda_min = 2*min(1,b)
lambda_max <- 2 * max(1, b)
lambda_min <- 2 * min(1, b)

# Condition number of the Hessian: kappa = lambda_max / lambda_min
kappa <- lambda_max / lambda_min
sqrt_kappa <- sqrt(kappa)

# Closed-form optimal hyperparameters for heavy-ball momentum,
# expressed in terms of the condition number kappa = lambda_max/lambda_min:
#   gamma* = ((sqrt(kappa) - 1) / (sqrt(kappa) + 1))^2
#   lr*    = 4 / (lambda_min * (sqrt(kappa) + 1)^2)
# (equivalent to the eigenvalue forms; lr* scales inversely with lambda_min,
#  while gamma* depends only on kappa.)
gamma <- ((sqrt_kappa - 1) / (sqrt_kappa + 1))^2
lr    <- 4 / (lambda_min * (sqrt_kappa + 1)^2)

# Define the function
f <- function(x, y) {
  return(x^2 + b * y^2)
}

# Define the gradient of the function
grad_f <- function(x, y) {
  df_dx <- 2 * x
  df_dy <- 2 * b * y
  return(c(df_dx, df_dy))
}

# Gradient Descent with Momentum (heavy-ball, standard form)
# x_{k+1} = x_k - lr * grad + gamma * (x_k - x_{k-1})
# This is the convention under which the closed-form optimal
# lr and gamma below are derived.
gradient_descent_2d_momentum <- function(start_x, start_y, max_iter) {
  x_prev <- start_x
  y_prev <- start_y
  x <- start_x
  y <- start_y

  # For tracking
  x_hist <- numeric(max_iter)
  y_hist <- numeric(max_iter)
  f_hist <- numeric(max_iter)

  for (i in 1:max_iter) {
    grad <- grad_f(x, y)
    dx <- grad[1]
    dy <- grad[2]

    # Heavy-ball update
    x_new <- x - lr * dx + gamma * (x - x_prev)
    y_new <- y - lr * dy + gamma * (y - y_prev)

    x_prev <- x
    y_prev <- y
    x <- x_new
    y <- y_new

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

# ===== Run Gradient Descent with Momentum =====
start_x <- 1
start_y <- 1
result <- gradient_descent_2d_momentum(start_x, start_y, max_iter)

# ===== Print Results =====
cat(sprintf("b (anisotropy)             : %.2f\n", b))
cat(sprintf("lambda_max / lambda_min    : %.4f / %.4f\n", lambda_max, lambda_min))
cat(sprintf("condition number (kappa)   : %.4f\n", kappa))
cat(sprintf("optimal step size (lr)     : %.4f\n", lr))
cat(sprintf("optimal momentum (gamma)   : %.4f\n", gamma))
cat(sprintf("Optimized x                : %.4f\n", result$optimal_x))
cat(sprintf("Optimized y                : %.4f\n", result$optimal_y))
cat(sprintf("Minimum f(x, y)            : %.4f\n", result$optimal_value))

# ===== Plot Trajectory in 2D =====
plot(result$x_history, result$y_history, type = 'b', col = 'blue',
     xlab = 'x', ylab = 'y',
     main = '2D Momentum Gradient Descent',
     xlim = range(c(result$x_history, 0)),
     ylim = range(c(result$y_history, 0)))
points(0, 0, col = 'red', pch = 19, cex = 1.5)
text(0, 0, labels = "Minimum (0,0)", pos = 4, col = 'red')
legend("topright", legend = c("Descent Path", "True Minimum (0,0)"),
       col = c("blue", "red"), pch = c(1, 19))

# ===== Plot Function Value Over Iterations =====
plot(1:max_iter, result$f_history, type = 'b', col = 'darkgreen',
     xlab = 'Iteration', ylab = 'f(x, y)',
     main = 'Function Value vs Iteration (Momentum)')
abline(h = 0, col = 'red', lty = 2)
legend("topright", legend = c("f(x, y)", "Optimum f = 0"),
       col = c("darkgreen", "red"), lty = c(1, 2))
