library(ggplot2)
library(tibble)

n <- 100L
x <- runif(100L, 2, 10)
y <- 10 + 2.5*x + 2*rnorm(n)
df <- data.table::as.data.table(cbind(x, y))

plot_points <- function(df, x_col, y_col, jitter = FALSE) {
  if (jitter) { # 扰动后散点图可以避免过多的点重合
    ggplot2::ggplot(data=df, mapping=ggplot2::aes(x=df[[x_col]], y = df[[y_col]])) +
      ggplot2::geom_jitter(alpha = 0.4)
  } else {
    ggplot2::ggplot(data=df, mapping=ggplot2::aes(x=df[[x_col]], y = df[[y_col]])) +
      ggplot2::geom_point()
  }
}

plot_points(df, "x", "y")
plot_points(df, "x", "y", TRUE)
