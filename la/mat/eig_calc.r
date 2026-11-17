##### demo: iterative eigenvalue computation via the QR algorithm
# The QR algorithm finds eigenvalues WITHOUT solving det(A - lambda I) = 0.
# Each step: factor  A_k = Q_k R_k   (QR decomposition), then form
#   A_{k+1} = R_k Q_k = Q_k^T A_k Q_k.
# This is a similarity transform, so eigenvalues are preserved, and A_k
# converges to (near) triangular form; its diagonal entries approach the
# eigenvalues. (For a symmetric A it becomes diagonal.)  Q_k is an ORTHOGONAL
# matrix at every step -- the very objects studied in orthogonal.r / svd.r.

A <- matrix(c(4, 1, 1, 3), nrow = 2)   # symmetric PD; eigenvalues 4.618, 2.382
A

##### basic QR iteration
Ak <- A
for (k in 1:30) {
  dec <- qr(Ak)
  Qk <- qr.Q(dec)          # orthogonal
  Rk <- qr.R(dec)          # upper triangular
  Ak <- Rk %*% Qk          # similarity transform: preserves eigenvalues
  if (k %in% c(1, 2, 3, 5, 10, 30)) {
    cat(sprintf("iter %2d: diag = (%.6f, %.6f)\n", k, Ak[1, 1], Ak[2, 2]))
  }
}
Ak                          # converged to diagonal -> eigenvalues on diagonal
diag(Ak)

# reference: base R's eigenvalue routine
eigen(A)$values

##### shifted QR iteration (practical variant, much faster convergence)
# Use the bottom-right entry as a Rayleigh shift sigma; factor
#   A_k - sigma I = Q R,  then  A_{k+1} = R Q + sigma I.
# Shifting removes the dominant eigenvalue first, giving cubic convergence
# for symmetric matrices.
Ak <- A
for (k in 1:10) {
  sigma <- Ak[nrow(Ak), ncol(Ak)]
  dec <- qr(Ak - sigma * diag(nrow(Ak)))
  Qk <- qr.Q(dec)
  Rk <- qr.R(dec)
  Ak <- Rk %*% Qk + sigma * diag(nrow(Ak))
  if (k %in% c(1, 2, 3, 5, 10)) {
    cat(sprintf("shifted iter %2d: diag = (%.8f, %.8f)\n",
                k, Ak[1, 1], Ak[2, 2]))
  }
}
diag(Ak)                    # matches eigen(A)$values to high precision
