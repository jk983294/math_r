##### positive definite matrix property
# A real symmetric matrix S is positive definite (PD) iff x^T S x > 0 for every
# nonzero x. Equivalent, easy-to-check tests:
#   - all eigenvalues > 0
#   - Sylvester's criterion: every leading principal minor > 0
#   - chol(S) succeeds (Cholesky requires positive definiteness)

S <- matrix(c(4, 1, 1, 3), nrow = 2)
S                       # symmetric
S - t(S)                # = 0 -> symmetric

eigen(S)$values         # both > 0  -> positive definite

# Sylvester's criterion: leading principal minors
S[1, 1]                 # 1x1 minor > 0
det(S)                  # 2x2 minor > 0

# x^T S x > 0 for every nonzero x (spot checks)
for (x in list(c(1, 0), c(1, 1), c(2, -1))) print(c(t(x) %*% S %*% x))

# Cholesky factorization (only exists for PD matrices)
R <- chol(S)            # upper triangular: S = R^T R
R
t(R) %*% R              # back to S
