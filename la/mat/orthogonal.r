# demo orthogonal matrix property
# Q^T * Q = I

get_rotation_matrix <- function(theta) {
  return(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2))
}
m1 <- get_rotation_matrix(pi / 3)
(t(m1) %*% m1)  # check orthogonality
(m1 %*% c(0, 1))  # rotate y axis
(m1 %*% c(1, 0))  # rotate x axis

####### reflection across the line through the origin at angle θ/2
get_reflextion_matrix <- function(theta) {
  return(matrix(c(cos(theta), sin(theta), sin(theta), -cos(theta)), nrow = 2))
}
m2 <- get_reflextion_matrix(pi / 3)
(t(m2) %*% m2)  # check orthogonality
(m2 %*% c(0, 1))  # reflex y axis
(m2 %*% c(1, 0))  # reflex x axis

###### Householder reflection
# A Householder reflection H reflects a vector x to align with -e1
# (i.e. zeros out every component except the first). It has the form
#   H = I - 2 uu^T / (u^T u),   u = x + sign(x1) ||x|| e1
# Like the 2x2 reflection, H is orthogonal, symmetric, H^2 = I, det = -1.
# Householder reflections are the building block of QR factorization.

householder <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  e1 <- c(1, rep(0, n - 1))
  alpha <- sign(x[1]) * sqrt(sum(x^2))   # = ±||x||
  u <- x + alpha * e1
  if (all(u == 0)) {
    return(diag(n))   # x already aligned with e1
  }
  H <- diag(n) - 2 * (u %*% t(u)) / sum(u^2)
  return(H)
}
x <- c(2, 1, 2)
H <- householder(x)
H                       # Householder matrix (symmetric, mirror)
t(H) %*% H              # orthogonal: H^T H = I
H %*% H                 # involution: H^2 = I
H %*% x                 # aligned with e1: ( -||x||, 0, 0 )

###### Hadamard matrix
# A Hadamard matrix H_n has entries +1/-1 and mutually orthogonal rows:
#   H_n^T H_n = n I_n
# So H_n / sqrt(n) is an orthogonal matrix. H_n is built recursively:
#   H_1 = [1],   H_{2n} = [ H_n  H_n ; H_n -H_n ]

hadamard <- function(k) {
  # returns the 2^k x 2^k Hadamard matrix (Sylvester construction)
  H <- matrix(1, 1, 1)
  for (i in seq_len(k)) {
    H <- rbind(cbind(H, H), cbind(H, -H))
  }
  return(H)
}

k <- 3
H <- hadamard(k)
H                       # 8x8 matrix of +1/-1
t(H) %*% H              # = 8 I_8  (rows mutually orthogonal)
(n <- 2^k)
Q <- H / sqrt(n)        # normalize
Q                       # normalized Hadamard
t(Q) %*% Q              # now exactly I: Q is orthogonal

##### Haar wavelet transform
# The Haar wavelet transform is an orthogonal transform: its matrix W is
# orthonormal, so W^T W = I. The rows are the Haar basis functions:
#   - row 1  : scaling (average) over the whole signal, value 1/sqrt(n)
#   - level l: step wavelets, +1/sqrt(2^l) on the left half of a block,
#              -1/sqrt(2^l) on the right half  (these are the "details")
# Applying W to a signal gives wavelet coefficients; W^T reconstructs it.

haar_matrix <- function(k) {
  n <- 2^k
  H <- matrix(0, n, n)
  H[1, ] <- rep(1 / sqrt(n), n)        # scaling / average row
  row <- 2
  for (level in 1:k) {
    block_size <- 2^level
    half <- block_size / 2
    val <- 1 / sqrt(block_size)
    for (b in 0:(n / block_size - 1)) {
      s <- b * block_size + 1
      m <- s + half
      e <- s + block_size - 1
      H[row, s:(m - 1)] <- val
      H[row, m:e] <- -val
      row <- row + 1
    }
  }
  return(H)
}

k <- 2
(W <- haar_matrix(k))   # 4x4 orthonormal Haar matrix
t(W) %*% W              # = I  (rows mutually orthogonal, norm 1)

x <- c(3, 1, 4, 2)      # demo signal
(coef <- W %*% x)       # wavelet coefficients (avg + 3 details)
t(W) %*% coef           # reconstruct: back to x


##### Discrete Fourier Transform
# The DFT is the complex analogue of the orthogonal transforms above. Its
# matrix F has entries  F[j,k] = exp(-2*pi*i*j*k/n),  j,k = 0..n-1.
# Its rows are orthogonal over C, so F %*% Conj(t(F)) = n I;  F/sqrt(n) is
# unitary (the complex version of an orthogonal matrix). Applying F to a
# signal gives frequency coefficients; reconstruct with (1/n) Conj(t(F)) %*% X.

dft_matrix <- function(n) {
  omega <- exp(-2i * pi / n)
  j <- 0:(n - 1)
  k <- 0:(n - 1)
  outer(j, k, function(a, b) omega^(a * b))
}

n <- 4
F <- dft_matrix(n)
Re(F)                   # real part of the 4x4 DFT matrix (imag part is +/-1,0)
round(F %*% Conj(t(F)), 10)   # = 4 I  (unitary: F F^H = n I)

x <- c(1, 2, 0, 1)      # demo signal
X <- F %*% x            # frequency coefficients (complex)
X
Re(Conj(t(F)) %*% X / n)   # reconstruct: back to x (real)

##### DFT basis = eigenvectors of a permutation matrix
# The same DFT matrix is just the eigenbasis of the cyclic-shift permutation
# matrix P: its columns are exactly P's eigenvectors, and the eigenvalues are
# the n-th roots of unity. So the DFT basis can be "discovered" by simply
# eigen-decomposing a permutation matrix -- no trig needed.

# cyclic down-shift permutation matrix: P e_j = e_{j-1}, P e_1 = e_n
P <- matrix(0, n, n)
for (m in 1:n) P[(m %% n) + 1, m] <- 1

E <- eigen(P)
lam <- E$values          # eigenvalues: the n-th roots of unity
V <- E$vectors           # eigenvectors, one per column (normalized to norm 1)

# reshape the eigenvectors to match the DFT matrix F:
#   scale to unit-modulus entries, strip per-column phase, order by eigenvalue
b_of <- function(z) {
  b <- round(Arg(z) * n / (2 * pi))
  ((b %% n) + n) %% n    # map angle -> column index 0..n-1
}
perm <- order(b_of(lam) + 1)   # inverse perm: place each eigenvector at its b
V2 <- V[, perm] * sqrt(n)
V2 <- sweep(V2, 2, V2[1, ], "/")   # each column now starts with 1, like F
max(abs(V2 - F))         # ~0 : eigenvectors == DFT columns (up to order/phase)

# equivalently, F diagonalizes P:  F^{-1} P F = diag(lam)
D <- (1 / n) * Conj(t(F)) %*% P %*% F
round(D, 10)             # diagonal -> confirms F is P's eigenbasis
