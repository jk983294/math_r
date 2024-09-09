library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)
d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

plot( Divorce ~ MedianAgeMarriage.s , d )
plot( Divorce ~ Marriage.s , d )

model <- rethinking::map(
    alist(
        Divorce ~ dnorm(mu, sigma),
        mu <- a + bR * Marriage.s + bA * MedianAgeMarriage.s,
        a ~ dnorm(10, 10),
        bR ~ dnorm(0, 1),
        bA ~ dnorm(0, 1),
        sigma ~ dunif(0, 10)
    ),
    data = d
)
rethinking::precis(model)
plot( rethinking::precis(model) )

# Counterfactual plot
# it is to see how the predictions change as you change only one predictor at a time.
A.avg <- mean( d$MedianAgeMarriage.s )
R.seq <- seq( from=-3 , to=3 , length.out=30 )
pred.data <- data.frame( Marriage.s=R.seq, MedianAgeMarriage.s=A.avg )
mu <- rethinking::link( model , data=pred.data ) # compute counterfactual mean divorce (mu)
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
R.sim <- rethinking::sim( model , data=pred.data , n=1e4 ) # simulate counterfactual divorce outcomes
R.PI <- apply( R.sim , 2 , PI )
# display predictions, hiding raw data with type="n"
plot( Divorce ~ Marriage.s , data=d , type="n" )
mtext( "MedianAgeMarriage.s = 0" )
lines( R.seq , mu.mean )
shade( mu.PI , R.seq )
shade( R.PI , R.seq )

# Posterior prediction plot
mu <- rethinking::link( model )
mu.mean <- apply( mu , 2 , mean ) # summarize samples across cases
mu.PI <- apply( mu , 2 , PI )
divorce.sim <- rethinking::sim( model , n=1e4 ) # again no new data, so uses original data
divorce.PI <- apply( divorce.sim , 2 , PI )
plot( mu.mean ~ d$Divorce , col=rangi2 , ylim=range(mu.PI) , xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) ) {
    lines( rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]) , col=rangi2 )
}
# compute residuals
divorce.resid <- d$Divorce - mu.mean
o <- order(divorce.resid) # get ordering by divorce rate
dotchart( divorce.resid[o] , labels=d$Loc[o] , xlim=c(-6,5) , cex=0.6 )
abline( v=0 , col=FC::color.alpha("black",0.2) )
for ( i in 1:nrow(d) ) {
    j <- o[i] # which State in order
    lines( d$Divorce[j]-c(mu.PI[1,j], mu.PI[2,j]) , rep(i,2) )
    points( d$Divorce[j]-c(divorce.PI[1,j],divorce.PI[2,j]) , rep(i,2), pch=3 , cex=0.6 , col="gray" )
}
