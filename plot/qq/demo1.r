library(ggplot2)
library(tibble)

plot_qq <- function(x){
  ggplot2::ggplot(data=tibble::tibble(x=x), mapping=ggplot2::aes(sample=x)) +
    ggplot2::stat_qq() +
    ggplot2::geom_qq_line(color="red")
}

plot_qq1 <- function(x){
  qqnorm(x)
  qqline(x, col = "blue", lwd = 2)
}

n <- 1000L
x <- rnorm(n, 0, 1)
x <- rlnorm(n, 0, 1) # log-normal
x <- -rexp(n) # negative exp dist
x <- rt(n, 2) # t dist with heavy tail
x <- c(rnorm(n, -1, .25), rnorm(n, 1, .25)) # bimodal

plot_hist(x, 30, TRUE)
plot_qq(x)
plot_qq1(x)
