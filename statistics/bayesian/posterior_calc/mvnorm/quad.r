library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# prior of µ ∼ Normal(178 , 20)
# prior of σ ∼ Uniform(0 , 50)
flist <- alist(
    height ~ dnorm( mu , sigma ),
    mu ~ dnorm( 178 , 20 ),
    sigma ~ dunif( 0 , 50 )
)

start <- list(mu=mean(d2$height), sigma=sd(d2$height)) # good guess of the rough location of the MAP values

model <- rethinking::map( flist , data=d2 , start=start)
rethinking::precis( model, prob=0.9 )
vcov( model ) # variance-covariance matrix
diag( vcov( model ) ) # variance
cov2cor( vcov( model ) ) # correlation matrix
post <- extract.samples( model , n=1e4 ) # samples from posterior
# post <- MASS::mvrnorm( n=1e4 , mu=coef(model) , Sigma=vcov(model) ) # samples from posterior
rethinking::precis(post)
