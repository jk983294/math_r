states <- c("s0", "s1", "s2")
actions <- c("a0", "a1")
P_a0 <- matrix( # action a0's transition matrix
    c(
        0.5, 0.5, 0,
        0.3, 0.7, 0,
        0, 0, 1
    ),
    nrow = 3, byrow = TRUE
)
P_a1 <- matrix( # action a1's transition matrix
    c(
        0.4, 0.6, 0,
        0.2, 0.8, 0,
        0, 0, 1
    ),
    nrow = 3, byrow = TRUE
)
tm <- list(P_a0, P_a1)
# Reward function, R(s0, a0, s1) = 2 means we are in state s0, and then take action a0 and land in state s1
R <- matrix(
    c(
        1, 2, 0, # a0 reward
        0, 3, 4 # a1 reward
    ),
    nrow = 2, byrow = TRUE
)
gamma <- 1.0 # discount factor

# Initialize the value function
V <- rep(0, length(states))
max_iterations <- 3

for (i in 1:max_iterations) {
    V_new <- rep(0, length(states))
    for (s in 1:length(states)) {
        a_values <- rep(0., length(states))
        for (a in 1:length(actions)) {
            # R[a, s] intermediate reward when take action a at state s
            t_matrix <- tm[[a]] # choose transition matrix associated with that action
            transition <- t_matrix[s, ] # choose transition vector based on current state s
            # transition %*% V is expected gain in one step
            a_values[a] <- R[a, s] + gamma * transition %*% V
        }
        V_new[s] <- max(a_values)
    }
    V <- V_new
}

# Print the value function
print("Value Function (Value Iteration):")
print(V)
