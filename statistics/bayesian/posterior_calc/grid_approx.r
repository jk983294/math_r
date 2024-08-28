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
