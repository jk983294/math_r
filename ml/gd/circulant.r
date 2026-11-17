# =============================================
# Convolution via a circulant matrix
#
# Circular convolution  y = k * x  (mod n) can be written as
# a matrix-vector product  y = C %*% x,  where C is the
# circulant matrix built from the kernel k:
#
#   first column of C = c(k, 0, ..., 0)
#   every next column   = previous column shifted down by one
#                         (wrapping around)
#
# This is the linear-algebra view of convolution used in CNNs
# (1D case) and in signal processing.
# =============================================


circulant_matrix <- function(k, n) {
  kern <- rep(0, n)
  m <- min(length(k), n)
  kern[1:m] <- k[1:m]
  C <- matrix(0, n, n)
  C[, 1] <- kern
  for (j in 2:n) {
    # shift previous column down by one, wrapping the last entry to the top
    prev <- C[, j - 1]
    C[, j] <- c(prev[n], prev[1:(n - 1)])
  }
  return(C)
}

# Direct circular convolution (definition), independent of the matrix form.
circular_convolve <- function(k, x) {
  n <- length(x)
  y <- numeric(n)
  for (i in 1:n) {
    for (j in 1:n) {
      idx <- ((i - j) %% n) + 1
      y[i] <- y[i] + k[j] * x[idx]
    }
  }
  return(y)
}

# ===== Example =====
x <- c(1, 2, 3, 4)        # signal
k <- c(1, -1, 0, 0)       # kernel (difference filter)

n <- length(x)
C <- circulant_matrix(k, n)

cat("Signal x:\n"); print(x)
cat("Kernel k:\n"); print(k)
cat("\nCirculant matrix C (first column = [k, 0, 0, 0]):\n")
print(C)

# Convolution as matrix multiplication
y_matmul <- as.vector(C %*% x)
# Convolution by direct definition
y_direct <- circular_convolve(k, x)

cat("\ny = C %*% x        :", y_matmul, "\n")
cat("y = direct convolve:", y_direct, "\n")
cat("Match:", isTRUE(all.equal(y_matmul, y_direct)), "\n")

# ===== Eigenvalues via FFT =====
# For a circulant matrix, eigenvalues are the DFT of the first column,
# and convolution is diagonalized by the Fourier basis:
#   C = F^-1 · diag(fft(k)) · F
lambda <- fft(C[, 1])
cat("\nEigenvalues of C (fft of first column):\n")
print(round(lambda, 6))

# Convolution via FFT: y = Re(ifft(fft(k) * fft(x)))
y_fft <- Re(fft(fft(k) * fft(x), inverse = TRUE) / n)
cat("y = ifft(fft(k) * fft(x)):", round(y_fft, 6), "\n")
cat("Match matrix form    :", isTRUE(all.equal(y_matmul, y_fft, tolerance = 1e-8)), "\n")

# ===== Sanity check on the eigenvalue claim =====
cat("\nCheck C %*% v = lambda * v for v = eigenvector from Fourier basis:\n")
v1 <- exp(2i * pi * (0:(n - 1)) / n)       # first Fourier mode
cv1 <- C %*% v1
cat("  ratio C v / v (should equal lambda[2]):\n")
print(round(as.vector(cv1 / v1), 6))
cat("  lambda[2]:", round(lambda[2], 6), "\n")
