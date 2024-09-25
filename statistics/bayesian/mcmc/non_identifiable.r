y <- rnorm( 100 , mean=0 , sd=1 )

model <- rethinking::ulam(
    alist(
        y ~ dnorm( mu , sigma ) ,
        mu <- a1 + a2 ,
        a1 ~ dnorm( 0 , 10 ) ,
        a2 ~ dnorm( 0 , 10 ) ,
        sigma ~ dcauchy( 0 , 1 )
    ) ,
    data=list(y=y) , start=list(a1=0, a2=0, sigma=1) , chains=2 , iter=4000 , warmup=1000 )

rethinking::precis(model)
