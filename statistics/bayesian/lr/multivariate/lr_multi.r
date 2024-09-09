library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)
d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

plot( Divorce ~ MedianAgeMarriage.s , d )
plot( Divorce ~ Marriage.s , d )

model <- rethinking::map(
    alist(
        Divorce ~ dnorm(mu, sigma),
        mu <- a + bR * Marriage.s + bA * MedianAgeMarriage.s,
        a ~ dnorm(10, 10),
        bR ~ dnorm(0, 1),
        bA ~ dnorm(0, 1),
        sigma ~ dunif(0, 10)
    ),
    data = d
)
rethinking::precis(model)
plot( rethinking::precis(model) )

# Predictor residual plot
# A predictor variable residual is the average prediction error when we use all of the other predictor variables to model a predictor of interest.
model_without_mr <- rethinking::map(
    alist(
    Marriage.s ~ dnorm( mu , sigma ) ,
    mu <- a + b*MedianAgeMarriage.s ,
    a ~ dnorm( 0 , 10 ) ,
    b ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
    ) , data = d )
beta <- c(coef(model_without_mr)['a'], coef(model_without_mr)['b'])
mu <- beta['a'] + beta['b']*d$MedianAgeMarriage.s
m.resid <- d$Marriage.s - mu # compute residual
df <- as.data.frame(cbind(x = d$MedianAgeMarriage.s, y = d$Marriage.s, resid=m.resid))
plot_residual(df, beta=beta)
