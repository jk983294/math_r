# symbol integral
D(expression(exp(x^2)), "x")  # exp(x^2) * (2 * x)

# numeric integral
integrate(function(x) x^2, 0, 1)  # 0.3333333
integrand <- function(x) 1/(x * x)
integrate(integrand, lower = 1, upper = Inf)  # 1

# integrand of gamma prob density function
a <- 3
b <- 2
integrand <- function(x) {
  return(x^(a - 1)*(1 - x)^(b - 1))
}
integrate(integrand, 0, 1)