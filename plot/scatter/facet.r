library(ggplot2)
library(data.table)

data <- data.table(
  x = rnorm(100),
  y = rnorm(100),
  group = rep(c("A", "B"), each = 50)
)

plot_scatter <- function(dt, x_col, y_col, facet_col = NA, color_col = NA) {
  if (is.character(facet_col)) {
    dt[, (facet_col) := as.factor(get(facet_col))]
    ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      facet_wrap(as.formula(paste("~", facet_col))) +
      theme_minimal()
  } else {
    if (is.character(color_col)) {
      dt[, (color_col) := as.factor(get(color_col))]
      ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[color_col]])) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        theme_minimal()
    } else {
      ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]])) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        theme_minimal()
    }
  }
}

plot_scatter(data, "x", "y")
plot_scatter(data, "x", "y", facet_col = "group")
plot_scatter(data, "x", "y", color_col = "group")
