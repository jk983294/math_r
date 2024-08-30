# Quadratic approximation (Gaussian approximation)

# (1) Find the posterior mode. algo find peaks.
# (2) Once you find the peak of the posterior, you must estimate the curvature near the peak.
# This curvature is sufficient to compute a quadratic approximation of the entire posterior distribution.

globe.qa <- rethinking::map( #  MAP stands for maximum a posterior
    alist(
        w ~ dbinom(9,p) , # binomial likelihood
        p ~ dunif(0,1) # uniform prior
    ),
    data=list(w=6))

# display summary of quadratic approximation
rethinking::precis( globe.qa )
