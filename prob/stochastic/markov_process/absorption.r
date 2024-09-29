# Time to absorption
# Multi-state models
# state 1 represents cirrhosis, state 2 denotes liver cancer, and state 3 is death

trials <- 100000
sim_list <- numeric(trials)
init <- 1 # initial state
for (i in 1:trials) {
    state <- init
    t <- 0
    while (TRUE) {
        if (state == 1) {
            q12 <- rexp(1, 0.0151)
            q13 <- rexp(1, 0.0071)
        }

        if (q12 < q13) {
            t <- t + q12
            state <- 2
        } else {
            t <- t + q13
            break
        }

        if (state == 2) {
            q23 <- rexp(1, 0.0284)
            t <- t + q23
            break
        }
    }
    sim_list[i] <- t
}
mean(sim_list)
