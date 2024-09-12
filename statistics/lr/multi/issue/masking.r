library(data.table)

N <- 100
rho <- 0.7 # correlation btw x_pos and x_neg
x_pos <- rnorm( N ) # x_pos as Gaussian
x_neg <- rnorm( N , rho*x_pos , sqrt(1-rho^2) ) # x_neg correlated with x_pos
y <- rnorm( N , x_pos - x_neg ) # y equally associated with x_pos, x_neg
d <- data.frame(y,x_pos,x_neg)
pairs(d)
FM::pcor(d)
model <- lm(y ~ x_pos + x_neg, d)
summary(model)

# As a result, (x_pos and x_neg) tend to cancel one another out.
model1 <- lm(y ~ x_pos, d)
summary(model1)
