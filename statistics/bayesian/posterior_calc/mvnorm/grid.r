library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# prior of µ ∼ Normal(178 , 20)
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
# prior of σ ∼ Uniform(0 , 50)
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
rethinking::dens( prior_h )

# grid approx for posterior
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) {
    sum(dnorm(d2$height, mean=post$mu[i] , sd=post$sigma[i], log=TRUE ))
})
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) + dunif( post$sigma ,  0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )
rethinking::contour_xyz( post$mu , post$sigma , post$prob )
rethinking::image_xyz( post$mu , post$sigma , post$prob )

# Sampling from the posterior
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE , prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 )
rethinking::dens( sample.mu )
rethinking::dens( sample.sigma )
rethinking::HPDI( sample.mu )
rethinking::HPDI( sample.sigma )
