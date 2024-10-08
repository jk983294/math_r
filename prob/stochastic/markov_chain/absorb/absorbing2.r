# expected number of flips to get HTHTH
# P(Heads) = 2/3, P(Tails) = 1/3

trials <- 100000
sim_list <- numeric(trials)
for (i in 1:trials) {
    pattern <- c(1, 0, 1, 0, 1) # 1: Heads, 0: Tails
    state <- sample(c(0, 1), 5, prob = c(1 / 3, 2 / 3), replace = T)
    k <- 5
    while (!prod(state == pattern)) {
        flip <- sample(c(0, 1), 1, prob = c(1 / 3, 2 / 3))
        state <- c(tail(state, 4), flip)
        k <- k + 1
    }
    sim_list[i] <- k
}
mean(sim_list)
# exact expectation = 38.625
