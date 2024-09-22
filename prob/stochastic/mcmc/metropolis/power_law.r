# Simulate power law simulation
# Power-law distributions:
# π(i) = i^(-3/2) / (∑ k^(-3/2)) for i = 1,2,...
# we use simple symmetric random walk (birth-death chain) on the positive integers with reflecting boundary at 1.
# From 1, the walk always moves to 2. Otherwise, the walk moves left or right with probability 1/2.

trials <- 1000000
sim_list <- numeric(trials)
sim_list[1] <- 2
for (i in 2:trials) {
    if (sim_list[i - 1] == 1) { # last state is 1, then go right always
        p <- (1 / 2)^(5 / 2)
        new <- sample(c(1, 2), 1, prob = c(1 - p, p))
        sim_list[i] <- new
    } else {
        left_right <- sample(c(-1, 1), 1) # choose evenly to go left or right
        if (left_right == -1) {
            sim_list[i] <- sim_list[i - 1] - 1
        } else {
            p <- (sim_list[i - 1] / (sim_list[i - 1] + 1))^(3 / 2)
            sim_list[i] <- sample(c(sim_list[i - 1], 1 + sim_list[i - 1]), 1, prob = c(1 - p, p))
        }
    }
}
dat <- sim_list[1000:trials] # discard first 1000
tab <- table(dat) / length(dat)
tab[1:10]
