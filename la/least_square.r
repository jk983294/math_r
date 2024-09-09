X <- cbind(c(1, 1, 1, 1, 1), c(1, 2, 3, 4, 5))
y <- c(3.7, 4.2, 4.9, 5.7, 6)

# y = a + bx beta = inv(X^T*X) * X^T * y
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat

# X = QR
z <- qr(X)
qr.Q(z) # 返回矩阵Q
qr.R(z) # 返回矩阵R
(beta_hat <- qr.solve(z, y))
beta_hat <- qr.coef(z, y)
qr.resid(z, y) # 残差
qr.fitted(z, y) # 拟合值
