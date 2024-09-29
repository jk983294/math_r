# simulate the maximum of a simple symmetric random walk

n <- 10000
sim_list <- replicate(
    10000,
    max(cumsum(sample(c(-1, 1), n, replace = T)))
)
mean(sim_list)
sd(sim_list)

sim <- replicate(
    10000,
    if (max(cumsum(sample(c(-1, 1), n, rep = T))) > 200) 1 else 0
)
mean(sim) # P(max > 200)
