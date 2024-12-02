library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )

model <- map(
    alist(
        admit ~ dbinom( applications , p ) ,
        logit(p) <- a + bm*male ,
        a ~ dnorm(0,10) ,
        bm ~ dnorm(0,10)
    ) ,
    data=d )
precis(model)
post <- extract.samples( model )
p.admit.male <- logistic( post$a + post$bm )
p.admit.female <- logistic( post$a )
diff.admit <- p.admit.male - p.admit.female
quantile( diff.admit , c(0.025,0.5,0.975) )

glm1 <- glm( cbind(admit,reject) ~ dept , data=d , family=binomial )
precis(glm1)
