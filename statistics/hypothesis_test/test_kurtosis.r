n <- 100L
x <- rnorm(n)
y <- runif(n)

FM::fm_kurt(x)

# check if kurt == 3, which is normal dist
print(moments::anscombe.test(x))
print(moments::anscombe.test(y))
