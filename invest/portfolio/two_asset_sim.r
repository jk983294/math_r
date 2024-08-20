mu <- c(0.02, 0.03)
rho <- -0.8
sd1 <- 0.1
sd2 <- 0.15
cov12 <- rho * sd1 * sd2
sigma <- matrix(c(sd1 * sd1, cov12, cov12, sd2 * sd2), nrow = 2, ncol = 2)
n <- 10^3
sim_ret <- mvtnorm::rmvnorm(n = n, mean = mu, sigma = sigma)
weight <- 0:100L / 100.

sim_pnls <- numeric(length(weight)) # n period sim pnl
vars <- numeric(length(weight)) # one period
rets <- numeric(length(weight)) # one period
for (i in 1:length(weight)) {
    w1 <- weight[i]
    w2 <- 1. - w1
    linear_ret <- w1 * (sim_ret[, 1]) + w2 * (sim_ret[, 2])
    cumulative_return <- cumprod(1 + linear_ret)
    sim_pnls[i] <- tail(cumulative_return, 1)
    rets[i] <- sum(c(w1, w2) * mu)
    vars[i] <- w1 * sd1 * sd1 + w2 * sd2 * sd2 + 2 * w1 * w2 * rho * sd1 * sd2
}
plot(weight, log(sim_pnls))
plot(vars, rets)
