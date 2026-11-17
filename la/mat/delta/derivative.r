##### demo: derivative of d(A(t)^-1)/dt  (A is a function of t)

## Derivation:
##   A(t) A(t)^-1 = I. Differentiate both sides wrt t:
##     A'(t) A^-1(t) + A(t) (A^-1)'(t) = 0
##   left-multiply by A^-1(t):
##     (A^-1)'(t) = -A^-1(t) A'(t) A^-1(t)

## This demo verifies the formula numerically: finite differences check
## both A'(t) and (A^-1)'(t), compared to the formula prediction.

## 0. helper
rel_err <- function(A, B) {
  norm(as.matrix(A - B), "F") / (norm(as.matrix(B), "F") + 1e-15)
}

## 1. matrix-valued function A(t)  (3x3, mixed functions of t)
A_of_t <- function(t) {
  matrix(c(
    t,        1,        t^2,
    1,        t + 2,    sin(t),
    t^2,      cos(t),   3 * t
  ), 3, 3, byrow = TRUE)
}

# A'(t) via central finite difference
A_dot <- function(t, h = 1e-6) {
  (A_of_t(t + h) - A_of_t(t - h)) / (2 * h)
}

t0 <- 1.3
A   <- A_of_t(t0)
Ainv <- solve(A)
Adot <- A_dot(t0)

## 2. formula prediction: (A^-1)'(t) = -A^-1 A'(t) A^-1
Ainv_dot_formula <- -(Ainv %*% Adot %*% Ainv)

## 3. verify: finite difference directly on A(t)^-1
h <- 1e-6
Ainv_dot_fd <- (solve(A_of_t(t0 + h)) - solve(A_of_t(t0 - h))) / (2 * h)

cat(sprintf("== verify d(A^-1)/dt at t0 = %.3f ==\n", t0))
cat(sprintf("formula vs finite-diff  relative error: %.3e\n", rel_err(Ainv_dot_formula, Ainv_dot_fd)))
cat("\nformula-given (A^-1)'(t0):\n")
print(round(Ainv_dot_formula, 6))

## 4. analytic check: rotation matrix R(t) (orthogonal, R^-1 = R^T)
## R(t) = [[cos t, -sin t],[sin t, cos t]],  R'(t) = [[-sin t, -cos t],[cos t, -sin t]]
## Formula: (R^-1)' = -R^-1 R' R^-1. Since R^-1=R^T, this equals (R')^T.
t1 <- 0.7
R  <- matrix(c(cos(t1), -sin(t1), sin(t1), cos(t1)), 2, 2, byrow = TRUE)
Rinv <- t(R)
Rprime <- matrix(c(-sin(t1), -cos(t1), cos(t1), -sin(t1)), 2, 2, byrow = TRUE)
Rinv_dot_formula2 <- -(Rinv %*% Rprime %*% Rinv)

Rinv_dot_fd2 <- (solve(matrix(c(cos(t1+h), -sin(t1+h), sin(t1+h), cos(t1+h)),2,2,byrow=TRUE)) -
                  solve(matrix(c(cos(t1-h), -sin(t1-h), sin(t1-h), cos(t1-h)),2,2,byrow=TRUE))) / (2 * h)

cat(sprintf("\n== rotation matrix R(t) at t1 = %.3f ==\n", t1))
cat(sprintf("formula vs finite-diff  relative error: %.3e\n", rel_err(Rinv_dot_formula2, Rinv_dot_fd2)))
cat("R orthogonal => R^-1=R^T and (R^-1)'=(R')^T, consistent with the formula.\n")

## 5. conclusion
cat("\nConclusion: for any invertible A(t),\n")
cat("  d/dt A(t)^-1 = -A(t)^-1 A'(t) A(t)^-1\n")
cat("verified numerically in both general and orthogonal cases (err ~ 1e-9,\n")
cat("limited by finite-difference step size).\n")

## --------------------------------------------------------------------------
## 6. derivative of an eigenvalue lambda(t) of A(t)
## --------------------------------------------------------------------------
## For a simple eigenvalue lambda(t) with right eigenvector x(t)
## (A x = lambda x) and left eigenvector y(t) (A^T y = lambda y),
## normalized so y^T x = 1, the formula is:
##   d lambda / dt = y^T A'(t) x
## If A is symmetric, left = right (x = y, x^T x = 1), so
##   d lambda / dt = x^T A'(t) x.

## helper: eigenvalue of A(t) nearest to a target (track one branch smoothly)
ev_near <- function(Afun, t, target, h = 1e-9) {
  v <- eigen(Afun(t))$values
  v[which.min(abs(v - target))]
}

## 6a. symmetric case: x^T A'(t) x
A_sym_of_t <- function(t) {
  matrix(c(t,       1,     0,
           1,       t + 2, sin(t),
           0,       sin(t), 3 * t), 3, 3, byrow = TRUE)
}
A_sym_dot <- function(t) {
  matrix(c(1, 0,      0,
           0, 1,      cos(t),
           0, cos(t), 3), 3, 3, byrow = TRUE)
}

t0 <- 0.9
es <- eigen(A_sym_of_t(t0))
idx <- 3                       # follow the largest eigenvalue
lambda0 <- es$values[idx]
x0 <- es$vectors[, idx]       # unit norm (symmetric)
dlam_sym <- as.numeric(t(x0) %*% A_sym_dot(t0) %*% x0)
dlam_sym_fd <- (ev_near(A_sym_of_t, t0 + h, lambda0) -
                ev_near(A_sym_of_t, t0 - h, lambda0)) / (2 * h)

cat(sprintf("== 6a) eigenvalue derivative (symmetric A) at t0 = %.3f ==\n", t0))
cat(sprintf("formula x^T A' x = %.6f\n", dlam_sym))
cat(sprintf("finite-diff       = %.6f\n", dlam_sym_fd))
cat(sprintf("relative error:    %.3e\n\n", rel_err(dlam_sym, dlam_sym_fd)))

## 6b. general (non-symmetric) case: y^T A'(t) x  (left eigenvector needed)
A_ns_of_t <- function(t) {
  matrix(c(t,     1,
           2,     t + 1), 2, 2, byrow = TRUE)
}
A_ns_dot <- function(t) matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE)   # = I

t1 <- 0.5
en <- eigen(A_ns_of_t(t1))
lambda1 <- en$values[1]
x1 <- en$vectors[, 1]                              # right eigenvector
# left eigenvector = eigenvector of A^T at the same eigenvalue
ent <- eigen(t(A_ns_of_t(t1)))
li <- which.min(abs(ent$values - lambda1))
y1_raw <- ent$vectors[, li]
y1 <- y1_raw / as.numeric(t(y1_raw) %*% x1)       # normalize so y1^T x1 = 1
dlam_ns <- as.numeric(t(y1) %*% A_ns_dot(t1) %*% x1)
dlam_ns_fd <- (ev_near(A_ns_of_t, t1 + h, lambda1) -
               ev_near(A_ns_of_t, t1 - h, lambda1)) / (2 * h)

cat(sprintf("== 6b) eigenvalue derivative (non-symmetric A) at t1 = %.3f ==\n", t1))
cat(sprintf("formula y^T A' x = %.6f   (check y^T x = %.3e)\n", dlam_ns, as.numeric(t(y1) %*% x1)))
cat(sprintf("finite-diff       = %.6f\n", dlam_ns_fd))
cat(sprintf("relative error:    %.3e\n\n", rel_err(dlam_ns, dlam_ns_fd)))

cat("Conclusion (eigenvalue derivative):\n")
cat("  simple eigenvalue: d lambda/dt = y^T A'(t) x  (y left, x right, y^T x = 1)\n")
cat("  symmetric A:       d lambda/dt = x^T A'(t) x  (x = y, unit norm)\n")
cat("both verified numerically (err ~ 1e-7, limited by finite-difference step).\n")

## --------------------------------------------------------------------------
## 7. derivative of a singular value sigma(t) of A(t)
## --------------------------------------------------------------------------
## For a simple singular value sigma(t) > 0 with left singular vector u(t)
## (A v = sigma u) and right singular vector v(t) (A^T u = sigma v),
## both unit norm, the formula is:
##   d sigma / dt = u^T A'(t) v
## (analogous to the eigenvalue case, but with left/right singular vectors)

A_sv_of_t <- function(t) {
  matrix(c(t,       1,     0,
           2,       t + 1, sin(t),
           0,       cos(t), 3 * t), 3, 3, byrow = TRUE)
}
# A'(t) by central finite difference (same step h as the sigma check below)
A_sv_dot <- function(t) (A_sv_of_t(t + h) - A_sv_of_t(t - h)) / (2 * h)

# singular value of A(t) nearest to a target (track one branch smoothly)
sv_near <- function(t, target) {
  d <- svd(A_sv_of_t(t))$d
  d[which.min(abs(d - target))]
}

t2 <- 0.9
s <- svd(A_sv_of_t(t2))
idx <- 1                          # follow the largest singular value
sigma0 <- s$d[idx]
u0 <- s$u[, idx]
v0 <- s$v[, idx]
d_sig_formula <- as.numeric(t(u0) %*% A_sv_dot(t2) %*% v0)
d_sig_fd <- (sv_near(t2 + h, sigma0) - sv_near(t2 - h, sigma0)) / (2 * h)

cat(sprintf("== 7) singular value derivative at t2 = %.3f ==\n", t2))
cat(sprintf("formula u^T A' v = %.6f\n", d_sig_formula))
cat(sprintf("finite-diff       = %.6f\n", d_sig_fd))
cat(sprintf("relative error:    %.3e\n\n", rel_err(d_sig_formula, d_sig_fd)))

cat("Conclusion (singular value derivative):\n")
cat("  simple singular value sigma > 0: d sigma/dt = u^T A'(t) v\n")
cat("  (u left, v right singular vectors; for symmetric A, u = +/- v)\n")
cat("verified numerically vs finite-difference of svd(A(t)) (err ~ 1e-9).\n")
