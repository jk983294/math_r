library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log( d$rgdppc_2000 ) # make log version of outcome
dd <- d[ complete.cases(d$rgdppc_2000) , ] # extract countries with GDP data
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]

model <- rethinking::ulam(
    alist(
        log_gdp ~ dnorm( mu , sigma ) ,
        mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
        a ~ dnorm( 0 , 100 ) ,
        bA ~ dnorm( 0 , 10 ) ,
        bR ~ dnorm( 0 , 10 ) ,
        bAR ~ dnorm( 0 , 10 ) ,
        sigma ~ dcauchy( 0 , 2 )
    ) ,
    data=dd.trim)
rethinking::precis(model)

model1 <- rethinking::ulam(model, chains = 4, cores = 4) # resample
post <- as.data.frame(rethinking::extract.samples( model ))
pairs(post)
show(model)
plot(model)
rethinking::stancode(model)
