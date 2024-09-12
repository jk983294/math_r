N <- 100
x0 <- rnorm(N,10,2)
treatment <- rep( 0:3 , each=N/4 )
y <- x0 + rnorm(N, 5 - 0.3 * treatment)
d <- data.frame( x0=x0 , y=y , treatment=treatment )

# automatically expand categorical factors into the necessary number of dummy variables
model <- lm(y ~ x0 + as.factor(treatment), d)
summary(model)
