p1 <- c(-2, 1, 1)  # -2 + x + x^2 = 0
roots <- polyroot(p1)
Mod(roots)  # absolute value of solution


Px = polynom::polynomial(c(-3, 2, 1)) # P(x) = -3 + 2x + x^2
polynom::poly.from.roots(c(-3, 1)) # (x - (-3)) * (x - (1))
as.character(Px)
coef(Px) # 提取多项式的升幂表示的各个系数为一个向量
curve(as.function(Px)(x), -4, 2, ylab="P(x)")
plot(Px, xlim=c(-5, 5))

# 加、减、乘法、除法
P1 <- polynom::polynomial(c(-2, 1))
print(P1 + Px)
print(P1 - Px)
print(P1 * Px)
print(Px / polynom::polynomial(c(3,1)))

deriv(Px) # 导数
polynom::integral(Px) # 不定积分
polynom::integral(Px, c(-3, 1)) # 求定积分

# 抛物线插值
ph <- polynom::poly.calc(x=c(1/16, 1/4, 1), y=sqrt(c(1/16, 1/4, 1)))
print(ph)
MASS::fractions(coef(ph)) # 分数表示
