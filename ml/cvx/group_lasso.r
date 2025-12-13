library(gglasso)

set.seed(123)
n <- 100
p <- 10
x <- matrix(rnorm(n*p), n, p)
y <- rnorm(n)

group <- rep(1:5, each = 2)
fit <- gglasso(x, y, group = group)
plot(fit)

cv_fit <- cv.gglasso(x, y, group = group)
plot(cv_fit)
best_lambda <- cv_fit$lambda.min
# 使用最优 lambda 重新拟合模型
best_fit <- gglasso(x, y, group = group, lambda = best_lambda)
