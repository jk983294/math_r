# Grid approximation
# (1) Define the grid. This means you decide how many points to use in estimating the posterior,
#     and then you make a list of the parameter values on the grid.
# (2) Compute the value of the prior at each parameter value on the grid.
# (3) Compute the likelihood at each parameter value.
# (4) Compute the unstandardized posterior at each parameter value, by multiplying the prior by the likelihood.
# (5) Finally, standardize the posterior, by dividing each value by the sum of all values.

grid_points <- 100L

# define grid of parameter p of Binom dist within [0, 1]
p_grid <- seq( from=0 , to=1 , length.out=grid_points )

# define prior (uniform/step/double exp)
prior <- rep( 1 , grid_points )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- exp( -5 * abs( p_grid - 0.5 ) )

# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid, posterior, type="b", xlab="probability of water" , ylab="posterior probability" )

# sample parameter from posterior
sample_n <- 1e4
samples <- sample(p_grid, prob=posterior, size=sample_n, replace=TRUE)
rethinking::dens(samples)
sum( samples < 0.5 ) / sample_n # Pr(p below 0.5)
sum( samples > 0.5 & samples < 0.75 ) / sample_n # Pr(p between 0.5 and 0.75)
quantile( samples , c( 0.025 , 0.975 ) ) # confidence interval
rethinking::HPDI(samples, prob=0.5) # highest posterior density interval (good for highly skewed posterior)
rethinking::chainmode( samples , adj=0.01 ) # MAP

# simulate data from posterior predictive distribution
# 1) sample parameter from posterior
# 2) sample data using sampled parameter
w <- rbinom( 1e4 , size=9 , prob=samples )
