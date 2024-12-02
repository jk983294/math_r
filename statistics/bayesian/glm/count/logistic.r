library(rethinking)
data(chimpanzees)
d <- chimpanzees

model <- map(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
        a ~ dnorm(0,10) ,
        bp ~ dnorm(0,10) ,
        bpC ~ dnorm(0,10)
    ),
    data=d )
precis(model)

d2 <- d
d2$recipient <- NULL
model1 <- rethinking::ulam( model , data=d2 , iter=1e4 , warmup=1000 )
precis(model1)
