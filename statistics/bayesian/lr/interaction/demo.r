library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log( d$rgdppc_2000 ) # make log version of outcome
dd <- d[ complete.cases(d$rgdppc_2000) , ] # extract countries with GDP data
# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ] # Africa
d.A0 <- dd[ dd$cont_africa==0 , ] # not Africa

m0 <- map(
    alist(
        log_gdp ~ dnorm( mu , sigma ) ,
        mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
        a ~ dnorm( 8 , 100 ) ,
        bA ~ dnorm( 0 , 1 ) ,
        bR ~ dnorm( 0 , 1 ) ,
        bAR ~ dnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 10 )
    ) ,
    data=dd )
