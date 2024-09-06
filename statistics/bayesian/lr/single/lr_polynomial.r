library(rethinking)
data(Howell1)
d <- Howell1
d$weight.s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight.s2 <- d$weight.s^2

# h(i) ∼ Normal(µ(i), σ)
# µ(i) = α + β1x(i) + β2x(i)^2
# α ∼ Normal(178, 100)
# β1 ∼ Normal(0, 10)
# β2 ∼ Normal(0, 10)
# σ ∼ Uniform(0, 50)
flist <- alist(
    height ~ dnorm( mu , sigma ),
    mu <- a + b1*weight.s + b2*weight.s2 ,
    a ~ dnorm( 156 , 100 ) ,
    b1 ~ dnorm( 0 , 10 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
)

model <- rethinking::map( flist , data=d)
rethinking::precis( model, prob=0.9 )
vcov( model ) # variance-covariance matrix
diag( vcov( model ) ) # variance
cov2cor( vcov( model ) ) # correlation matrix

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight.s=weight.seq , weight.s2=weight.seq^2 )
mu <- rethinking::link( model , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , rethinking::PI , prob=0.89 )
sim.height <- rethinking::sim( model , data=pred_dat )
height.PI <- apply( sim.height , 2 , rethinking::PI , prob=0.89 )

plot( height ~ weight.s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )
