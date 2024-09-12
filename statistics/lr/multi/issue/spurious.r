library(data.table)

N <- 100
x_real <- rnorm( N ) # x_real as Gaussian with mean 0 and stddev 1
x_spur <- rnorm( N , x_real ) # x_spur as Gaussian with mean=x_real
y <- rnorm( N , x_real ) # y as Gaussian with mean=x_real
d <- as.data.table(cbind(y, x_real, x_spur))
pairs(d)
FM::pcor(d)
lm(y ~ x_real + x_spur, d)
