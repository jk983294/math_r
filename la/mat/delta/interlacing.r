##### demo: interlacing of eigenvalues (Cauchy interlacing theorem)
#####
##### Cauchy interlacing theorem:
#####   Let A be a real symmetric n x n matrix and B a principal submatrix of
#####   order n-1 (delete one row and the matching column). With eigenvalues
#####   sorted ascending,
#####     lam_1(A) <= lam_1(B) <= lam_2(A) <= lam_2(B) <= ...
#####              <= lam_{n-1}(A) <= lam_{n-1}(B) <= lam_n(A)
#####   Deleting r rows/cols gives r "gaps":
#####     lam_j(A) <= mu_j(B) <= lam_{j+r}(A),   j = 1..n-r.
#####
##### Closely related (Weyl / rank-1 perturbation): for c > 0, the eigenvalues
##### nu of A + c u u^T (a positive semidefinite rank-1 update) interlace those
##### of A as
#####     lam_1 <= nu_1 <= lam_2 <= nu_2 <= ... <= lam_n <= nu_n
##### (for c < 0 the order reverses). All are consequences of the
##### Courant-Fischer min-max principle.

set.seed(123)
n <- 6
M <- matrix(rnorm(n * n), n, n)
A <- M %*% t(M) + n * diag(n)            # symmetric positive definite
lamA <- sort(eigen(A, symmetric = TRUE)$values)
cat(sprintf("A is %d x %d, eigenvalues (ascending):\n", n, n))
cat(sprintf("  %.4f", lamA), "\n", sep = "")

## helper: interlacing for an order n-1 principal submatrix (lamB shorter by 1)
check_interlace1 <- function(lamA, lamB, tol = 1e-8) {
  ok <- TRUE
  for (k in seq_along(lamB)) {
    if (lamA[k]     - tol > lamB[k])     ok <- FALSE
    if (lamB[k]     - tol > lamA[k + 1]) ok <- FALSE
  }
  ok
}

cat("\n== delete one row/column: eigenvalues of B interlace those of A ==\n")
all_ok <- TRUE
for (i in 1:n) {
  B <- A[-i, -i]
  lamB <- sort(eigen(B, symmetric = TRUE)$values)
  ok <- check_interlace1(lamA, lamB)
  all_ok <- all_ok && ok
  if (i == 1) {
    parts <- c()
    for (k in 1:(n - 1)) parts <- c(parts, sprintf("%.3f", lamA[k]), sprintf("%.3f", lamB[k]))
    parts <- c(parts, sprintf("%.3f", lamA[n]))
    chain <- paste(parts, collapse = " <= ")
    cat("  chain (delete row/col 1):", chain, "\n")
  }
  cat(sprintf("  delete index %d : interlacing holds = %s\n", i, ok))
}
cat(sprintf("  ALL single-deletion interlacing checks: %s\n\n", all_ok))

## general r-deletion interlacing: lam_j <= mu_j <= lam_{j+r}
check_interlace_r <- function(lamA, lamB, r, tol = 1e-8) {
  ok <- TRUE
  for (j in seq_along(lamB)) {
    if (lamA[j]      - tol > lamB[j])     ok <- FALSE
    if (lamB[j]      - tol > lamA[j + r]) ok <- FALSE
  }
  ok
}

r <- 2
idx <- 1:r
B2 <- A[-idx, -idx]
lamB2 <- sort(eigen(B2, symmetric = TRUE)$values)
ok2 <- check_interlace_r(lamA, lamB2, r)
cat(sprintf("== delete %d rows/cols (indices %s): lam_j <= mu_j <= lam_{j+%d} ==\n",
            r, paste(idx, collapse = ","), r))
cat(sprintf("  interlacing holds = %s\n\n", ok2))

## rank-1 perturbation (c > 0): lam_1 <= nu_1 <= lam_2 <= ... <= lam_n <= nu_n
set.seed(7)
u <- rnorm(n); u <- u / sqrt(sum(u * u))    # unit vector
cc <- 3
A1 <- A + cc * (u %*% t(u))
lam1 <- sort(eigen(A1, symmetric = TRUE)$values)
chain1 <- c(rbind(lamA, lam1))               # lam_1, nu_1, lam_2, nu_2, ...
ok1 <- all(diff(chain1) >= -1e-8)
cat(sprintf("== rank-1 update A + %d u u^T (c>0): nu_j interlace lam_j ==\n", cc))
cat(sprintf("  interlacing holds = %s\n", ok1))
cat(sprintf("  (check: lam_1(A)=%.3f <= nu_1=%.3f <= lam_2(A)=%.3f)\n",
            lamA[1], lam1[1], lamA[2]))

cat("\nConclusion:\n")
cat("  Cauchy interlacing holds for any principal submatrix (deleting r rows/cols\n")
cat("  creates r gaps: lam_j <= mu_j <= lam_{j+r}). A rank-1 perturbation shows\n")
cat("  the same interleaving pattern, all by the Courant-Fischer min-max principle.\n")
