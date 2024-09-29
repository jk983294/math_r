trials <- 10000
sim_list <- numeric(trials)
t <- 0.95
for (i in 1:trials) {
    rw <- c(0, cumsum(sample(c(-1, 1), 10000, replace = T)))
    # zero occur not at last [0.95, 1.0] steps
    sim_list[i] <- if (0 == length(which(rw[(10000 * t):10000] == 0))) 1 else 0
}
mean(sim_list) # P(L_n < t*n), L_n is the last time, in n plays, that the players are tied

# last 0 occur time histogram
trials <- 10000
sim_list <- numeric(trials)
for (i in 1:trials) {
    rw <- c(0, cumsum(sample(c(-1, 1), (trials - 1), replace = T)))
    sim_list[i] <- tail(which(rw == 0), 1)
}
mean(sim_list)
hist(sim_list, xlab = "", ylab = "Counts", main = "")
