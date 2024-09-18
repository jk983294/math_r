# Markov Reward Processes
# Markov Decision Processes
# compute the value function V(s) for each state.
# The value function V(s) represents the expected cumulative discounted reward starting from state s.
states <- c("s0", "s1", "s2") # finite set of states
R <- c(1, 2, 3) # reward function
P <- matrix(
    c(
        0.5, 0.3, 0.2,
        0.1, 0.6, 0.3,
        0.2, 0.2, 0.6
    ),
    nrow = 3, byrow = TRUE
) # state transition probability matrix
gamma <- 0.9 # discount factor

# Initialize the value function
V <- rep(0, length(states))

# Policy Evaluation
max_iterations <- 1000
tolerance <- 1e-6

for (i in 1:max_iterations) {
    V_new <- R + gamma * P %*% V
    if (max(abs(V_new - V)) < tolerance) {
        break
    }
    V <- V_new
}

# Print the value function
print("Value Function (Policy Evaluation):")
print(V)
