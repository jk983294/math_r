mu <- c(0.05, 0.10, 0.15)  # Expected returns
# Covariance matrix
sigma <- matrix(c(0.0025, 0.0010, 0.0005,
                  0.0010, 0.0100, 0.0018,
                  0.0005, 0.0018, 0.0225), nrow = 3, byrow = TRUE)

# Define Constraints:
# Fully invested portfolio: sum of weights = 1
# Long-only positions: weights >= 0
A <- matrix(c(1, 1, 1,  # Sum of weights = 1
              1, 0, 0,  # Weight of asset 1 >= 0
              0, 1, 0,  # Weight of asset 2 >= 0
              0, 0, 1), # Weight of asset 3 >= 0
              nrow = 4, byrow = TRUE)
b <- c(1, 0, 0, 0)  # Right-hand side of constraints

result <- quadprog::solve.QP(Dmat = 2 * sigma, dvec = mu, Amat = t(A), bvec = b, meq = 1)
weights <- result$solution