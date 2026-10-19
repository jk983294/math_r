##### symmetric matrix property
# A real symmetric matrix equals its own transpose: S = S^T, so S - t(S) = 0.
# By the spectral theorem it is orthogonally diagonalizable:
#   S = Q Lambda Q^T,  with Q orthogonal (Q^T Q = I) and Lambda diagonal.
# So a symmetric matrix can be "rotated" by an orthogonal Q into a diagonal
# matrix of its real eigenvalues.

S <- matrix(c(0, 1, 1, 0), nrow = 2)
S - t(S)                # = 0  -> confirms S is symmetric

# spectral decomposition via eigen() (symmetric -> real vals, orthogonal vecs)
E <- eigen(S)
(lam <- E$values)       # eigenvalues (real)
(Q <- E$vectors)        # eigenvectors as columns
t(Q) %*% Q              # = I  -> eigenvector matrix is orthogonal

# reconstruct: S = Q Lambda Q^T, equivalently Q^T S Q = Lambda (diagonal)
round(t(Q) %*% S %*% Q, 10)    # diagonal: the eigenvalues of S
Q %*% diag(lam) %*% t(Q)       # back to S
