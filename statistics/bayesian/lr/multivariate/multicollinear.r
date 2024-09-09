library(data.table)

N <- 100
y <- rnorm(N,10,2)
leg_prop <- runif(N, 0.4, 0.5)
x1 <- leg_prop * y + rnorm( N , 0 , 0.02 )
x2 <- leg_prop * y + rnorm( N , 0 , 0.02 )
d <- data.frame(y, x1, x2)

model <- rethinking::map(
    alist(
        y ~ dnorm( mu , sigma ) ,
        mu <- a + b1*x1 + b2*x2 ,
        a ~ dnorm( 10 , 100 ) ,
        b1 ~ dnorm( 2 , 10 ) ,
        b2 ~ dnorm( 2 , 10 ) ,
        sigma ~ dunif( 0 , 10 )
    ),
    data=d )
rethinking::precis(model)
plot(rethinking::precis(model))

post <- rethinking::extract.samples(model)
plot( b1 ~ b2 , post , col=FC::color.alpha("#8080FF", 0.1) , pch=16 )

sum_bs <- post$b1 + post$b2
rethinking::dens( sum_bs , col="#8080FF" , lwd=2 , xlab="sum of b1 and b2" )
