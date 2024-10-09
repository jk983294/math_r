# First hitting time for Brownian motion with drift

mu <- 1 / 2
sig <- 1
a <- 10
sim_n <- 5000L
sim_list <- numeric(sim_n)
for (i in 1:sim_n) {
    t <- 80
    n <- 10000
    bm <- c(0, cumsum(rnorm(n, 0, sqrt(t / n))))
    xproc <- mu * seq(0, t, t / n) + sig * bm
    sim_list[i] <- which(xproc >= a)[1] * (t / n)
}
mean(sim_list, na.rm = TRUE)
var(sim_list, na.rm = TRUE)

# Exact results are mean = 20 and variance = 80
# E(T) = a / u = 20 and Var(T) = a * sigma^2 ∕ u^3 = 80
