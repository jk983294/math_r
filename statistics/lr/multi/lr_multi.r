n <- 10000L # number of observation
p <- 10L # number of predictor

x <- matrix(rnorm(n * p, 0, 1), nrow=n, ncol=p)
y <- apply(x[, 1:p], 1, sum) + rnorm(n)

XtX <- t(x) %*% x
Xy <- t(x) %*% y
beta <- solve(XtX) %*% Xy
