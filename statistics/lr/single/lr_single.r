n <- 10000L
x <- rnorm(n, 0, 1)
noise <- rnorm(n, 0, 1)
y <- 2. * x + noise

(cor <- FM::pcor(x, y))

# show that:
# slope(xy_model) * slope(yx_model) = cor * cor = R-squared
xy_model <- lm(y ~ 0 + x)
yx_model <- lm(x ~ 0 + y)
summary(xy_model)
summary(yx_model)
