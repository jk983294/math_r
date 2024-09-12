library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
d2$weight.c <- d2$weight - mean(d2$weight) # centering

# h(i) ∼ Normal(µ(i), σ)
# µ(i) = α + βx(i)
# α ∼ Normal(178, 100)
# β ∼ Normal(0, 10)
# σ ∼ Uniform(0, 50)
flist <- alist(
    height ~ dnorm( mu , sigma ),
    # mu <- a + b*weight.c ,
    mu <- a + b*weight ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
)

start <- list(a=mean(d2$height), b=0., sigma=sd(d2$height))

model <- rethinking::map( flist , data=d2 , start=start)
rethinking::precis( model, prob=0.9 )
vcov( model ) # variance-covariance matrix
diag( vcov( model ) ) # variance
# if fit with demean weight, then correlation between intercept and slope go away
cov2cor( vcov( model ) ) # correlation matrix

# plot MAP line
plot( height ~ weight , data=d2 )
abline( a=coef(model)["a"] , b=coef(model)["b"] )

# plot MAP line with interval
mu <- rethinking::link( model ) # sample and calc Xtβ for every weight in d2
weight.seq <- seq( from=25 , to=70 , by=1 )
my_link <- function(weight, post) post$a + post$b*weight
mu <- link( model , data=data.frame(weight=weight.seq) ) # sample and calc Xtβ for given weight
plot( height ~ weight , d2 , type="n" ) # use type="n" to hide raw data
for ( i in 1:100 ) { # plot the distribution of µ values at each x
    points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )
}
(mu.mean <- apply( mu , 2 , mean )) # summarize the distribution of mu
(mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 ))
plot( height ~ weight , data=d2 )
lines( weight.seq , mu.mean )
shade( mu.HPDI , weight.seq )

# sample from posterior distribution
post <- rethinking::extract.samples( model, n=1e4 )
plot( d2$weight , d2$height , xlim=range(d2$weight) , ylim=range(d2$height) , xlab="weight" , ylab="height" )
# plot sample lines, with transparency
for ( i in 1:100L ) {
    abline( a=post$a[i] , b=post$b[i] , col=rethinking::col.alpha("black", 0.3) )
}

# sample y, not distributions of plausible average y, µ
my_sim <- sapply( weight.seq , function(weight) rnorm(n=nrow(post), mean=post$a + post$b*weight, sd=post$sigma) )
sim.height <- rethinking::sim( model , data=list(weight=weight.seq) )
height.PI <- apply( sim.height , 2 , rethinking::PI , prob=0.89 )
plot( height ~ weight , d2 )
lines( weight.seq , mu.mean ) # draw MAP line
shade( mu.HPDI , weight.seq ) # draw HPDI region for line
shade( height.PI , weight.seq ) # draw PI region for simulated heights

# calculate WAIC
n_samples <- 10000L
ll <- sapply( 1:n_samples ,
    function(s) {
        mu <- post$a[s] + post$b[s] * d2$weight
        dnorm( d2$height , mu , post$sigma[s] , log=TRUE )
    } )
n_cases <- nrow(d2)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(ll[i,]) - log(n_samples) )
pWAIC <- sapply( 1:n_cases , function(i) var(ll[i,]) )
WAIC <- -2*( sum(lppd) - sum(pWAIC) )
WAIC_sd <- sqrt( n_cases*var(-2*( lppd - pWAIC )) )
