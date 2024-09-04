n <- 10000L # number of observation
p <- 10L # number of predictor
true_intercept <- 2.
fit_intercept <- TRUE

X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
y <- apply(X[, 1:p], 1, sum) + rnorm(n)
if (fit_intercept) {
    X <- cbind(1, X) # add a column of 1s for the intercept
    y <- y + true_intercept
}


XtX <- t(X) %*% X
inverse_XtX <- solve(XtX)
Xy <- t(X) %*% y
beta_hat <- inverse_XtX %*% Xy
y_hat <- X %*% beta_hat # calculate the predicted values
residuals <- y - y_hat
sigma_square_hat <- sum(residuals^2) / (nrow(X) - ncol(X))


TSS <- sum((y - mean(y))^2) # the total sum of squares (TSS)
RSS <- sum(residuals^2) # the residual sum of squares (RSS)
(R_squared <- 1 - (RSS / TSS))

# calculate the covariance matrix of the coefficients
cov_matrix <- inverse_XtX * sigma_square_hat
std_errors <- sqrt(diag(cov_matrix)) # extract the standard errors
print(std_errors)

# calculate t-values
(t_values <- beta_hat / std_errors)

# calculate p-values of coef
# large p-values means beta[j] = 0
(p_values <- 2 * pt(-abs(t_values), df = nrow(X) - ncol(X)))

# check the normality of residuals
qqnorm(residuals)
qqline(residuals, col = "red")
