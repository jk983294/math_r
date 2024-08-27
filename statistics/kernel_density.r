# comparing kernel density plots, draw them in same chart
x <- rnorm(100, 0, 1)
y <- rnorm(126, 0.3, 1.2)
z <- rnorm(93, -0.5, 0.7)
xlim <- range(x, y, z)
dx <- density(x, from = xlim[1], to = xlim[2], n = 200)
dy <- density(y, from = xlim[1], to = xlim[2], n = 200)
dz <- density(z, from = xlim[1], to = xlim[2], n = 200)

ylim <- range(dx$y, dy$y, dz$y)
plot(dx$x, dx$y, col = 1, lwd = 2, type = "l", xlim = xlim, ylim = ylim)
lines(dy$x, dy$y, col = 2, lwd = 2)
lines(dz$x, dz$y, col = 3, lwd = 2)