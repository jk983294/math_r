# 1. Simulate the number of arrivals N in [0, t] from a Poisson distribution with parameter λt.
# 2. Generate N i.i.d. random variables uniformly distributed on (0, t).
# 3. Sort the variables in increasing order to give the Poisson arrival times.

t <- 40
lambda <- 1 / 2
N <- rpois(1, lambda * t)
unifs <- runif(N, 0, t)
arrivals <- sort(unifs)
arrivals
