library(data.table)

len <- 1000L
n_path <- 10 # 10 paths
dt <- as.data.table(lapply(1:10, function(idx) cumsum(rnorm(len, 0, 0.001))))

plot_ts <- function(dt, first_n = 0L, last_n = 0L) {
    highlight_n <- first_n + last_n
    col = hcl.colors(10, "Temps")
    if (highlight_n > 0) {
        matplot(dt, type = "l", col = "lightgray", lty = 1)
        if (first_n > 0) {
            matlines(dt[, 1:first_n], type = "l", col = col, lty = 1)
        }
        if (last_n > 0) {
            matlines(dt[, (length(dt) - last_n + 1):length(dt)], type = "l", col = col, lty = 1)
        }
    } else {
        matplot(dt, type = "l", col = col, lwd = 2, lty = 1)
    }
}

plot_ts(dt)
plot_ts(dt, first_n = 2)
plot_ts(dt, last_n = 2)
plot_ts(dt, first_n = 1, last_n = 1)

ts.plot(dt, gpars= list(col=rainbow(2)))
