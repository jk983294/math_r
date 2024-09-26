# Gibbs sampler for bivariate normal distribution
rho <- -0.80
sd1 <- 1.2
sd2 <- 1.3
mu1 <- 1.5
mu2 <- 1.2
trials <- 5000
burn_trials <- trials / 5
sim_list <- matrix(rep(0, 2 * trials), ncol = 2)

for (i in 2:trials) { # start from (0, 0), so i start from 2
    # updates switch back and forth
    x2 <- sim_list[i - 1, 2]
    mu1_given_x2 <- mu1 + rho * (x2 - mu2) * sd1 / sd2
    sd1_given_x2 <- sd1 * sd1 * (1 - rho * rho)
    sim_list[i, 1] <- rnorm(1, mu1_given_x2, sd1_given_x2)

    x1 <- sim_list[i, 1]
    mu2_given_x1 <- mu2 + rho * (x1 - mu1) * sd2 / sd1
    sd2_given_x1 <- sd2 * sd2 * (1 - rho * rho)
    sim_list[i, 2] <- rnorm(1, mu2_given_x1, sd2_given_x1)
}

sim_list <- sim_list[burn_trials:trials, ]
cov(sim_list)
cov2cor(cov(sim_list))

plot(sim_list, pch = 20, xlab = "x", ylab = "y", main = "")
