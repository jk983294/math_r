# matrix_power(mat,k) = mat^k
matrix_power <- function(mat, k) {
    if (k == 0) {
        return(diag(dim(mat)[1]))
    }
    if (k == 1) {
        return(mat)
    }
    if (k > 1) {
        return(mat %*% matrix_power(mat, k - 1))
    }
}

###### Simulate discrete-time Markov chain ########################
# Simulates n steps of a Markov chain
# sim_markov(init, mat, n, states)
# Generates X0, ..., Xn for a Markov chain
# init: initial distribution
# mat: transition matrix
sim_markov <- function(init, mat, n, labels) {
    if (missing(labels)) labels <- 1:length(init)
    sim_list <- numeric(n + 1)
    states <- 1:length(init)
    sim_list[1] <- sample(states, 1, prob = init)
    for (i in 2:(n + 1)) {
        sim_list[i] <- sample(states, 1, prob = mat[sim_list[i - 1], ])
    }
    labels[sim_list]
}

### Stationary distribution of discrete-time Markov chain
###  (uses eigenvectors)
calc_stationary <- function(mat) {
    x <- eigen(t(mat))$vectors[, 1]
    as.double(x / sum(x)) # normalized so that components sum to 1
}
