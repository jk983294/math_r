library(Matrix)

# create by 列向量
A <- t(matrix(c(3, -7, -2, 2, -3, 5, 1, 0, 6, -4, 0, -5, -9, 5, -5, 12), 4, 4))

# PA = LU
res <- Matrix::expand(Matrix::lu(A))
res$L

# Cholesky
n <- 3
A <- matrix(rnorm(n^2), nrow = n)
cov_matrix <- t(A) %*% A  # This will be a positive-definite matrix
U <- chol(cov_matrix)
print("Upper Triangular Matrix (Cholesky Factor):")
print(U)
reconstructed_matrix <- t(U) %*% U

# QR
z <- qr(A)
qr.Q(z) # 返回矩阵Q
qr.R(z) # 返回矩阵R

# SVD
svd(A)
