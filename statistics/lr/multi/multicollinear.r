library(data.table)

N <- 100
y <- rnorm(N,10,2)
leg_prop <- runif(N, 0.4, 0.5)
x1 <- leg_prop * y + rnorm( N , 0 , 0.02 )
x2 <- leg_prop * y + rnorm( N , 0 , 0.02 )
d <- data.frame(y, x1, x2)

pairs(d)
FM::pcor(d)

# When two predictor variables are very strongly correlated, including both in a model may lead to confusion!!!
model <- lm(y ~ x1 + x2, d)
cov2cor(vcov(model))
summary(model)

model1 <- lm(y ~ x1, d)
summary(model1)
