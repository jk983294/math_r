# Simulate continuous-time Markov chain with Poisson subordination

generator_mat <- matrix(c(
    -2, 1, 1,
    1, -3, 2,
    0, 1, -1
), nrow = 3, byrow = T)
colnames(generator_mat) <- 1:3
rownames(generator_mat) <- 1:3
lambda <- 3
r <- (1 / lambda) * generator_mat + diag(3)

target_time <- 2
init_state <- 1
trials <- 100000
sim_list <- numeric(trials) # store state when time exceed target_time
for (i in 1:trials) {
    s <- 0 # time
    state <- init_state
    new_state <- init_state
    while (s < target_time) {
        state <- new_state
        s <- s + rexp(1, lambda) # wait at state
        new_state <- sample(1:3, 1, prob = r[state, ]) # choose new_state to visit next
    }
    sim_list[i] <- state
}

table(sim_list) / trials
round(expm::expm(target_time * generator_mat), 4)
