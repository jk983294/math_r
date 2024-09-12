N <- 100
x0 <- rnorm(N,10,2)
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
y <- x0 + rnorm(N, 5 - 3*fungus)
d <- data.frame( x0=x0 , y=y , treatment=treatment , fungus=fungus )

model <- lm(y ~ x0 + treatment, d)
summary(model)

# The problem is that fungus is mostly a consequence of treatment.
# This is to say that fungus is a post-treatment variable.
# fungus mask treatment
model <- lm(y ~ x0 + treatment + fungus, d)
summary(model)
