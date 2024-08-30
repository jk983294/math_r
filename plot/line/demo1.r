library(ggplot2)
library(tibble)

n <- 100L
x <- runif(100L, 2, 10)
y <- 10 + 2.5*x + 2*rnorm(n)
df <- data.table::as.data.table(cbind(x, y))

plot_line <- function(df, x_col, y_col, jitter = FALSE) {
  ggplot2::ggplot(data=df, mapping=ggplot2::aes(x=df[[x_col]])) +
    ggplot2::geom_line(mapping=ggplot2::aes(y=df[[y_col]]), col="red") +
    ggplot2::geom_hline(yintercept=0) +
    ggplot2::geom_vline(xintercept=0)
}

plot_line(df, "x", "y")
