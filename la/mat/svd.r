##### demo Singular Value Decomposition (SVD)
# For any real m x n matrix A, the SVD writes
#   A = U Sigma V^T
# where U (m x m) and V (n x n) are ORTHOGONAL, and Sigma is m x n diagonal
# with the non-negative singular values on its diagonal. SVD works for ANY
# matrix (square or rectangular, symmetric or not); its factors U and V are
# exactly the orthogonal matrices studied in orthogonal.r.

(A <- matrix(1:6, nrow = 3, ncol = 2))   # a rectangular 3x2 matrix

# ask for the FULL decomposition (U square) so t(U) %*% U = I holds as claimed
# (by default svd() returns the thin form, making U skinny, not square)
decomp <- svd(A, nu = nrow(A), nv = ncol(A))
(U <- decomp$u)          # 3x3 orthogonal
(d <- decomp$d)          # singular values (length 2)
(V <- decomp$v)          # 2x2 orthogonal

t(U) %*% U             # = I  -> U orthogonal
t(V) %*% V             # = I  -> V orthogonal

# reconstruct A from its SVD: U Sigma V^T
Sigma <- matrix(0, 3, 2); diag(Sigma) <- d   # 3x2 diagonal w/ singular values
U %*% Sigma %*% t(V)                         # back to A

# singular values squared = eigenvalues of A^T A (and of A A^T)
eigen(t(A) %*% A)$values   # = d^2 (up to ordering)
d^2

# low-rank approximation: keep only the largest singular value (rank 1)
k <- 1
Ak <- U[, 1:k] %*% diag(d[1:k], nrow = k, ncol = k) %*% t(V[, 1:k])
Ak                        # rank-1 approximation of A
