source("~/github/math_r/prob/stochastic/sp_utils.r")

m <- rbind(
    c(0.575, 0.118, 0.172, 0.109, 0.026),
    c(0.453, 0.243, 0.148, 0.123, 0.033),
    c(0.104, 0.343, 0.367, 0.167, 0.019),
    c(0.015, 0.066, 0.318, 0.505, 0.096),
    c(0.000, 0.060, 0.149, 0.567, 0.224)
)

# method 1
(m18 <- matrix_power(m, 18))
(stable_dist <- m18[1, ])

# method 2
init_ <- rep(1 / nrow(m), nrow(m))
results <- sim_markov(init_, m, 1e6)
(stable_dist <- table(results) / length(results))

# method 3
# eigen method for ergodic unichain mc
(stable_dist <- calc_stationary(m))
calc_stationary(m) %*% m # check that it is a stationary distribution by one step further
eigens <- eigen(t(m))
ev <- eigens$vectors[, 1]
(stable_dist <- as.double(ev / sum(ev)))

# converge_rate
(all_evs <- abs(eigens$values))
(converge_rate <- all_evs[2]) # second max eigen value
