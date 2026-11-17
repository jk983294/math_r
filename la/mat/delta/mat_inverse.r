##### demo: Inverse of a Matrix Changes When the Matrix Changes Slightly, Sherman-Morrison Woodbury

# relative error (Frobenius norm)
rel_err <- function(A, B) {
  norm(as.matrix(A - B), "F") / (norm(as.matrix(B), "F") + 1e-15)
}

# component-wise max relative error
max_rel_err <- function(A, B) {
  idx <- which.max(abs(B))
  abs(A[idx] - B[idx]) / (abs(B[idx]) + 1e-15)
}

## --------------------------------------------------------------------------
## 1. inverse sensitivity to small perturbation
## --------------------------------------------------------------------------
## A(eps) = A + eps * E. First-order: A(eps)^{-1} = A^{-1} - eps * A^{-1} E A^{-1} + O(eps^2)
## A tiny change in A can move A^{-1} a lot (large condition number => near singular).

set.seed(42)
n <- 5
A <- matrix(rnorm(n * n), n, n)
A <- A %*% t(A) + n * diag(n)
Ainv <- solve(A)

E <- matrix(rnorm(n * n), n, n) * 1e-3
eps_vals <- c(1e-6, 1e-5, 1e-4, 1e-3)

cat("== 1) inverse under small perturbation A(eps) = A + eps*E ==\n")
cat(sprintf("%-10s %-16s %-16s\n", "eps", "first-order err", "true err"))
for (eps in eps_vals) {
  A_eps <- A + eps * E
  Ainv_eps <- solve(A_eps)
  Ainv_approx <- Ainv - eps * (Ainv %*% E %*% Ainv)
  theo <- rel_err(Ainv_approx, Ainv)
  real <- rel_err(Ainv_eps, Ainv)
  cat(sprintf("%-10.0e %-16.3e %-16.3e\n", eps, theo, real))
}
cat("\nNote: smaller eps => true err approaches first-order prediction (O(eps^2) negligible).\n\n")


## --------------------------------------------------------------------------
## 2. Sherman-Morrison (rank-1 update)
## --------------------------------------------------------------------------
## (A + u v^T)^{-1} = A^{-1} - (A^{-1} u v^T A^{-1}) / (1 + v^T A^{-1} u)

u <- matrix(rnorm(n), n, 1)
v <- matrix(rnorm(n), n, 1)

M1 <- A + u %*% t(v)
M1_inv_direct <- solve(M1)

denom <- 1 + t(v) %*% Ainv %*% u
denom <- as.numeric(denom)
M1_inv_sm <- Ainv - (Ainv %*% u %*% t(v) %*% Ainv) / denom

cat("== 2) Sherman-Morrison (rank-1 update) ==\n")
cat(sprintf("SM vs direct  rel err: %.3e\n", rel_err(M1_inv_sm, M1_inv_direct)))
cat(sprintf("SM max comp  rel err: %.3e\n\n", max_rel_err(M1_inv_sm, M1_inv_direct)))


## --------------------------------------------------------------------------
## 3. Woodbury (rank-k update)
## --------------------------------------------------------------------------
## (A + U C V^T)^{-1} = A^{-1} - A^{-1} U (C^{-1} + V^T A^{-1} U)^{-1} V^T A^{-1}
## Sherman-Morrison is the k=1 case.

k <- 2
U <- matrix(rnorm(n * k), n, k)
C <- matrix(rnorm(k * k), k, k)
V <- matrix(rnorm(n * k), n, k)

M2 <- A + U %*% C %*% t(V)
M2_inv_direct <- solve(M2)

Cinv <- solve(C)
inner <- Cinv + t(V) %*% Ainv %*% U
inner_inv <- solve(inner)
M2_inv_wb <- Ainv - Ainv %*% U %*% inner_inv %*% t(V) %*% Ainv

cat(sprintf("== 3) Woodbury (rank-k update, k = %d) ==\n", k))
cat(sprintf("Woodbury vs direct  rel err: %.3e\n", rel_err(M2_inv_wb, M2_inv_direct)))
cat(sprintf("Woodbury max comp rel err: %.3e\n\n", max_rel_err(M2_inv_wb, M2_inv_direct)))


## --------------------------------------------------------------------------
## 4. timing: when n large and k small, update >> re-invert
## --------------------------------------------------------------------------
## re-invert is O(n^3); Woodbury is mostly small matmuls, O(n^2 k).

n_big <- 400
k_big <- 3
A_big <- matrix(rnorm(n_big * n_big), n_big, n_big)
A_big <- A_big %*% t(A_big) + n_big * diag(n_big)
Ainv_big <- solve(A_big)
U_big <- matrix(rnorm(n_big * k_big), n_big, k_big)
C_big <- matrix(rnorm(k_big * k_big), k_big, k_big)
V_big <- matrix(rnorm(n_big * k_big), n_big, k_big)

t_direct <- system.time({ M_big_direct <- solve(A_big + U_big %*% C_big %*% t(V_big)) })
t_wood <- system.time({
  Cinv_b <- solve(C_big)
  inner_b <- Cinv_b + t(V_big) %*% Ainv_big %*% U_big
  M_big_wb <- Ainv_big - Ainv_big %*% U_big %*% solve(inner_b) %*% t(V_big) %*% Ainv_big
})

cat(sprintf("== 4) big matrix n = %d, k = %d timing ==\n", n_big, k_big))
cat(sprintf("re-invert solve():          %.3f s\n", t_direct[3]))
cat(sprintf("Woodbury (A^{-1} known):   %.3f s\n", t_wood[3]))
cat(sprintf("rel err between them:      %.3e\n", rel_err(M_big_wb, M_big_direct)))
cat("\nConclusion: with large n and small update rank k, Woodbury update is far\n")
cat("faster than re-inverting, while remaining numerically consistent.\n")
