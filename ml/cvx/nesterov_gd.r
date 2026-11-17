# =============================================
# 2D Nesterov Accelerated Gradient (NAG)
# Function: f(x, y) = x^2 + b * y^2
# Minimum at: x = 0, y = 0, f(x,y) = 0
# Nesterov "lookahead" form:
#   v_{k+1} = gamma * v_k - lr * grad_f(x_k + gamma * v_k)
#   x_{k+1} = x_k + v_{k+1}
# For a quadratic the optimal lr and gamma are the same
# closed forms as the heavy-ball method:
#   lr*    = (2 / (sqrt(lambda_max) + sqrt(lambda_min)))^2
#   gamma* = ((sqrt(lambda_max) - sqrt(lambda_min)) /
#             (sqrt(lambda_max) + sqrt(lambda_min)))^2reframe
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

# Closed-form optimal hyperparameters for Nesterov (same forms as
# heavy-ball), expressed in terms of the condition number kappa:
#   gamma* = ((sqrt(kappa) - 1) / (sqrt(kappa) + 1))^2
#   lr*    = 4 / (lambda_min * (sqrt(kappa) + 1)^2)
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

# Nesterov Accelerated Gradient (lookahead form)
gradient_descent_2d_nesterov <- function(start_x, start_y, max_iter) {
  x <- start_x
  y <- start_y
  vx <- 0
  vy <- 0

  # For tracking
  x_hist <- numeric(max_iter)
  y_hist <- numeric(max_iter)
  f_hist <- numeric(max_iter)

  for (i in 1:max_iter) {
    # Evaluate gradient at the "lookahead" point
    grad <- grad_f(x + gamma * vx, y + gamma * vy)
    dx <- grad[1]
    dy <- grad[2]

    # Nesterov update
    vx_new <- gamma * vx - lr * dx
    vy_new <- gamma * vy - lr * dy

    x <- x + vx_new
    y <- y + vy_new
    vx <- vx_new
    vy <- vy_new

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

# ===== Run Nesterov =====
start_x <- 1
start_y <- 1
result <- gradient_descent_2d_nesterov(start_x, start_y, max_iter)

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
     main = '2D Nesterov Accelerated Gradient',
     xlim = range(c(result$x_history, 0)),
     ylim = range(c(result$y_history, 0)))
points(0, 0, col = 'red', pch = 19, cex = 1.5)
text(0, 0, labels = "Minimum (0,0)", pos = 4, col = 'red')
legend("topright", legend = c("Descent Path", "True Minimum (0,0)"),
       col = c("blue", "red"), pch = c(1, 19))

# ===== Plot Function Value Over Iterations =====
plot(1:max_iter, result$f_history, type = 'b', col = 'darkgreen',
     xlab = 'Iteration', ylab = 'f(x, y)',
     main = 'Function Value vs Iteration (Nesterov)')
abline(h = 0, col = 'red', lty = 2)
legend("topright", legend = c("f(x, y)", "Optimum f = 0"),
       col = c("darkgreen", "red"), lty = c(1, 2))
