f <- function(x) 1/4*x[1]^4 + 1/2*x[2]^2 - x[1]*x[2] + x[1] - x[2]
optim(c(0, 0), f) # 多元函数无约束最优化
optim(c(0, 0), f, method="BFGS")


f <- function(x) {
  return(x^3 - 2*x - 5)
}
interval <- c(1, 3)
root <- uniroot(f, interval)
root_value <- root$root
print(paste("The root is approximately:", root_value))
