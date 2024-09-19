library(rethinking)
data(milk)
d <- milk[ complete.cases(milk) , ]
d$neocortex <- d$neocortex.perc / 100
dim(d)

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))
m0 <- map(
    alist(
        kcal.per.g ~ dnorm( a , exp(log.sigma) )
    ) ,
    data=d , start=list(a=a.start,log.sigma=sigma.start) )
m1 <- map(
    alist(
        kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
        mu <- a + bn*neocortex
    ) ,
    data=d , start=list(a=a.start,bn=0,log.sigma=sigma.start) )
m2 <- map(
    alist(
        kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
        mu <- a + bm*log(mass)
    ) ,
    data=d , start=list(a=a.start,bm=0,log.sigma=sigma.start) )
m3 <- map(
    alist(
        kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
        mu <- a + bn*neocortex + bm*log(mass)
    ) ,
    data=d , start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start) )

# Comparing WAIC values, Smaller values are better.
rethinking::WAIC( m0 )
( milk.models <- rethinking::compare( m0 , m1 , m2 , m3 ) )
plot( milk.models , SE=TRUE , dSE=TRUE )
rethinking::coeftab(m0 , m1 , m2 , m3)
plot( rethinking::coeftab(m0 , m1 , m2 , m3) )


# model averaging
# combines the results according to Akaike weights
nc.seq <- seq(from=0.5,to=0.8,length.out=30)
d.predict <- list(
    kcal.per.g = rep(0,30), # empty outcome
    neocortex = nc.seq, # sequence of neocortex
    mass = rep(4.5,30) # average mass
)
milk.ensemble <- ensemble( m0 , m1 , m2 , m3 , data=d.predict )
mu <- apply( milk.ensemble$link , 2 , mean )
mu.PI <- apply( milk.ensemble$link , 2 , PI )