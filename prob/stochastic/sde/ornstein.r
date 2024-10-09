# Ornstein-Uhlenbeck simulation
# dX_t = −r(X_t − u)dt + sigma * dB_t

xo <- 2
mu <- -1
sig <- .1
alpha <- .5
mesh <- 100
t <- 10
bm <- c(0, cumsum(rnorm(1000))) / sqrt(100)
xlist <- numeric(1001)
for (i in 0:1000) {
    xlist[i + 1] <- mu + (xo - mu) * exp(-alpha * i / 100) +
        sig * bm[i + 1] - alpha * sig * exp(-alpha * i / 100) *
            sum(exp(alpha * (0:i) / 100) * bm[1:(i + 1)] / 1000)
}
plot(seq(0, 10, .01), xlist, type = "l", ylim = c(-2, 2), xlab = "", ylab = "")
abline(h = -1, lty = 2)


# simulation of Ornstein-Uhlenbeck with Euler-Maruyama method
mu <- -1
r <- 0.5
sigma <- 0.1
T <- 10
n <- 1000
xpath <- numeric(n + 1)
xpath[1] <- 2 # initial vlaue
for (i in 1:n) {
    xpath[i + 1] <- xpath[i] - r * (xpath[i] - mu) * T / n +
        sigma * sqrt(T / n) * rnorm(1)
}
plot(seq(0, T, T / n), xpath, type = "l")

# For one outcome of X_T code simplifies
x <- -2
for (i in 1:n) {
    x <- x - r * (x - mu) * T / n + sigma * sqrt(T / n) * rnorm(1)
}
x

# simulation of mean and standard deviation of X_10
x <- -2
trials <- 10000
sim_list <- numeric(trials)
for (k in 1:trials) {
    for (i in 1:n) {
        x <- x - r * (x - mu) * T / n + sigma * sqrt(T / n) * rnorm(1)
    }
    sim_list[k] <- x
}
mean(sim_list)
sd(sim_list)
