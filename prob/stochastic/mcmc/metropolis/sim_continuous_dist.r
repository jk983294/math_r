# simulate a standard normal random variable
# From state s, the proposal chain moves to t, where t is uniformly distributed on (s − 1, s + 1)

mu <- 1.5
sigma <- 2
trials <- 1000000
sim_list <- numeric(trials)
state <- 0
for (i in 2:trials) {
    proposal <- runif(1, state - 1, state + 1)
    acceptance <- exp(-((proposal - mu)^2 - (state - mu)^2) / (2 * sigma^2))
    if (runif(1) < acceptance) state <- proposal
    sim_list[i] <- state
}
mean(sim_list)
sd(sim_list)
hist(sim_list, xlab = "", main = "", prob = T)
curve(dnorm(x, mean = mu, sd = sigma), mu - 4, mu + 4, add = T)
