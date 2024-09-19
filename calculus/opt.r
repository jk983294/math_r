# 多元函数无约束最优化
f <- function(x) 1/4*x[1]^4 + 1/2*x[2]^2 - x[1]*x[2] + x[1] - x[2]
optim(c(0, 0), f) # 多元函数无约束最优化
optim(c(0, 0), f, method="BFGS")

# 在指定区间求根
f <- function(x) {
  return(x^3 - 2*x - 5)
}
interval <- c(1, 3)
root <- uniroot(f, interval) # Brent method
root_value <- root$root
print(paste("The root is approximately:", root_value))

# 一元函数优化求解
h <- function(x) x^5 * exp(-x)
optimize(h, lower = 0, upper = 20, maximum = TRUE)
optimize(h, lower = 0, upper = 20, maximum = FALSE)

# minimizing a non-linear function
f <- function(x) {
  return((x - 2)^2 + 1)
}
result <- nlm(f, p = 0)  # Starting guess is 0
print(result$estimate)  # The value of x that minimizes the function
print(result$minimum)   # The minimum value of the function
