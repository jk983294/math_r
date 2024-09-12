sppnames <- c( "afarensis","africanus","habilis","boisei", "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

FM::pcor(brainvolcc, masskg)

model <- lm( brain ~ mass , data=d )
summary(model)
(r_square <- 1 - var(resid(model))/var(d$brain))
(deviance <- (-2) * logLik(model))

# compute deviance
theta <- coef(model)
sigma_hat <- sd(resid(model))
deviance <- (-2)*sum( dnorm( d$brain , mean=theta[1]+theta[2]*d$mass, sd=sigma_hat , log=TRUE ) )

# While deviance on training data always improves with additional predictor variables,
# deviance on future data may or may not
# AIC = D(train) + 2p