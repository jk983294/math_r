# Simulating graduation, drop-out rate
init <- c(0, 1, 0, 0, 0, 0) # student starts as fresh
P <- matrix(c(
    0.03, 0.91, 0.00, 0.00, 0.06, 0.00,
    0.00, 0.03, 0.91, 0.00, 0.06, 0.00,
    0.00, 0.00, 0.03, 0.93, 0.04, 0.00,
    0.00, 0.00, 0.00, 0.03, 0.04, 0.93,
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
), nrow = 6, byrow = T)
states <- c("Freshman", "Sophomore", "Junior", "Senior", "Drop", "Grad")
rownames(P) <- states
colnames(P) <- states
sim_markov(init, P, 10, states)
sim <- replicate(10000, sim_markov(init, P, 10, states)[11])
table(sim) / 10000


Q <- P[1:4, 1:4]
R <- P[1:4, 5:6]
f <- solve(diag(4) - Q)
(Absorb <- f %*% R) # probability
f %*% rep(1, 4)  # expected time
