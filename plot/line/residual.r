n <- 100L
x <- runif(100L, 2, 10)
y <- 10 + 2.5 * x + 2 * rnorm(n)
df <- data.table::as.data.table(cbind(x, y))
beta <- c(10, 2.5)
df[, resid := y - (beta[[1]] + beta[[2]] * x)]

plot_residual <- function(df, beta) {
    plot(y ~ x, df)
    abline(a = beta[[1]], b = beta[[2]])
    for (i in 1:length(resid)) {
        x_ <- df$x[i]
        y_ <- df$y[i]
        lines(c(x_, x_), c(y_ - df$resid[i], y_), lwd = 0.5, col = FC::color.alpha("black", 0.7))
    }
}

plot_residual(df, beta)
