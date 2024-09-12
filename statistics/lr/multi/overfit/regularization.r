n <- 10000L # number of observation
p <- 2L # number of predictor
true_intercept <- 2.

X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
y <- apply(X[, 1:p], 1, sum) + rnorm(n) + true_intercept
X <- as.data.frame(X)
df <- cbind(X, y)

model <- MASS::lm.ridge(y ~ V1 + V2, data = df, lambda = 1.)
