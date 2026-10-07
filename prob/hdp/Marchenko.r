library(data.table)

set.seed(42)

n <- 10000
p <- 100
gamma <- p / n # aspect ratio p/n

X <- matrix(rnorm(n * p), nrow = n, ncol = p)

cat("Matrix dimensions:", dim(X), "\n")
cat("Mean of each column (should be ~0):", round(mean(colMeans(X)), 5), "\n")
cat("Var  of each column (should be ~1):", round(mean(apply(X, 2, var)), 5), "\n\n")

cov_mat <- cov(X)

cat("Covariance matrix dimensions:", dim(cov_mat), "\n\n")
eig_result <- eigen(cov_mat, symmetric = TRUE)
eigenvalues  <- eig_result$values
eigenvectors <- eig_result$vectors

cat("Number of eigenvalues:", length(eigenvalues), "\n\n")

dt <- data.table(
  Index       = 1:p,
  Eigenvalue  = eigenvalues,
  Pct_Variance = eigenvalues / sum(eigenvalues) * 100,
  Cum_Pct      = cumsum(eigenvalues) / sum(eigenvalues) * 100
)

cat("=== Top 10 eigenvalues ===\n")
print(round(dt[1:10, ], 4))

cat("\n=== Bottom 5 eigenvalues ===\n")
print(round(dt[(p-4):p, ], 6))

cat("\nSum of all eigenvalues (total variance):", round(sum(eigenvalues), 4), "\n")
cat("Trace of covariance matrix (should match):", round(sum(diag(cov_mat)), 4), "\n\n")

plot(1:p, eigenvalues, type = "b", pch = 19, cex = 0.6,
     xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Scree Plot — Eigenvalues of Covariance Matrix")
abline(h = 1, col = "red", lty = 2)  # Kaiser criterion line (since vars are standardized-ish)
legend("topright", legend = "Kaiser criterion (λ=1)",
       col = "red", lty = 2, bty = "n")

# Sample covariance matrix S = X'X / n  (p x p)
S <- crossprod(X) / n

# Eigenvalues of S
eig <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
eig <- sort(eig)

# Theoretical MP density
mp_density <- function(x, gamma) {
  if (gamma <= 0) stop("gamma must be > 0")
  lam_min <- (1 - sqrt(gamma))^2
  lam_max <- (1 + sqrt(gamma))^2
  out <- ifelse(
    x >= lam_min & x <= lam_max,
    sqrt((lam_max - x) * (x - lam_min)) / (2 * pi * gamma * x),
    0
  )
  out
}

# Histogram of empirical eigenvalues
hist(eig,
     breaks = 60,
     freq = FALSE,
     col = "lightblue",
     border = "white",
     main = sprintf("Marchenko–Pastur law (gamma = p/n = %.2f)", gamma),
     xlab = "Eigenvalue of sample covariance matrix",
     xlim = c(0, (1 + sqrt(gamma))^2 * 1.1))

# Overlay theoretical density
curve(mp_density(x, gamma),
      from = (1 - sqrt(gamma))^2,
      to   = (1 + sqrt(gamma))^2,
      col = "red", lwd = 2, add = TRUE)

# Support lines
abline(v = (1 - sqrt(gamma))^2, lty = 2)
abline(v = (1 + sqrt(gamma))^2, lty = 2)
legend("topright",
       legend = c("Empirical eigenvalues", "MP density",
                  "MP support bounds"),
       col = c("lightblue", "red", "black"),
       lty = c(NA, 1, 2),
       pch = c(15, NA, NA),
       bty = "n")

