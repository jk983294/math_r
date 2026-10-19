##### demo of different matrix norms
# Common matrix norms:
#   Frobenius  ||A||_F  = sqrt(sum a_ij^2)          = sqrt(trace(A^T A))
#   1-norm     ||A||_1  = max absolute column sum
#   infinity   ||A||_inf = max absolute row sum
#   spectral   ||A||_2  = largest singular value (operator norm induced by l2)
#   nuclear    ||A||_*  = sum of singular values (trace norm; dual of ||.||_2)
#   max-norm   ||A||_M  = largest |entry|

# R's norm() covers 'F', '1'/'O' (col sum), 'I' (row sum) and 'M'.
# The spectral and nuclear norms are not in norm(); take them from svd().

A <- matrix(c(1, -2, 3, 4), nrow = 2, byrow = TRUE)
A

# Frobenius norm
norm(A, 'F')
sqrt(sum(A^2))                # same, by definition
sqrt(sum(svd(A)$d^2))         # also = sqrt(sum of squared singular values)

# 1-norm: maximum absolute column sum
norm(A, '1')
max(colSums(abs(A)))

# infinity-norm: maximum absolute row sum
norm(A, 'I')
max(rowSums(abs(A)))

# max-norm (largest entry in absolute value)
norm(A, 'M')

# spectral norm = largest singular value
svd(A)$d[1]                   # ||A||_2

# nuclear (trace) norm = sum of singular values; it is the convex surrogate
# for rank and the dual of the spectral norm (max over vs sum over sigma_i)
sum(svd(A)$d)                 # ||A||_*

# these three SVD-based norms always satisfy:  ||A||_2 <= ||A||_F <= ||A||_*
svd(A)$d[1]                   # spectral
sqrt(sum(svd(A)$d^2))         # Frobenius
nuclear <- sum(svd(A)$d); nuclear   # nuclear

# --- connection to orthogonal matrices (orthogonal.r) ---
# An orthogonal matrix Q has ||Q||_2 = 1 and PRESERVES the Frobenius norm,
# whether multiplied on the left or the right:
Q <- matrix(c(0, 1, 1, 0), nrow = 2)   # the swap matrix from symmetric.r
norm(Q, '2')                  # = 1  (spectral norm of an orthogonal matrix)
norm(Q %*% A, 'F')            # = ||A||_F  (rotation/reflection keeps length)
norm(A %*% Q, 'F')            # = ||A||_F
