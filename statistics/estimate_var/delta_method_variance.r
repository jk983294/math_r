# Define the function as a formula
g <- ~ x1^2 + x2
means <- c(x1 = 1, x2 = 2)
covariance_matrix <- matrix(c(0.1, 0.05, 0.05, 0.2), nrow = 2, byrow = TRUE)

# Apply the Delta method
variance_g <- msm::deltamethod(g, means, covariance_matrix)
print(variance_g)
