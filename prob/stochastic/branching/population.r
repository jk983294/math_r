# Branching process with different offspring distributions

### Poisson offspring distribution
branch_pois <- function(n_gen, mu) {
    lam <- mu
    z <- c(1, rep(0, n_gen)) # total people in generation z[i]
    for (i in 2:(n_gen + 1)) {
        z[i] <- sum(rpois(z[i - 1], lam))
    }
    return(z)
}

### Geometric distribution
branch_geom <- function(n, mu) {
    p <- 1. / (1 + mu)
    z <- c(1, rep(0, n))
    for (i in 2:(n + 1)) {
        z[i] <- sum(rgeom(z[i - 1], p))
    }
    return(z)
}

## offspring_dist[i] means has i-1 child prob
branch_vec <- function(n, offspring_dist) {
    z <- c(1, rep(0, n))
    for (i in 2:(n + 1)) {
        z[i] <- sum(sample(0:2, z[i - 1], replace = T, prob = offspring_dist))
    }
    return(z)
}

### Examples
mean_generation <- 1 / 4 # Extinction
mean_generation <- 1. # stable
mean_generation <- 1.2 # explode
offspring_dist <- c(1 / 6, 1 / 3, 1 / 2)
n_generation <- 10L
n_sim <- 5000L
sim_list <- replicate(n_sim, branch_pois(n_generation, mean_generation)[n_generation + 1L])
sim_list <- replicate(n_sim, branch_geom(n_generation, mean_generation)[n_generation + 1L])
sim_list <- replicate(n_sim, branch_vec(n_generation, prob_vec)[n_generation + 1L])
(mean_population_at_last <- mean(sim_list))
(sd_of_population_at_last <- sd(sim_list))
(extinction_prob <- sum(sim_list == 0) / n_sim)
