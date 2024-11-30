library(ggplot2)

data <- data.frame(
    category = c("A", "B", "C", "D", "A", "B", "C", "D"),
    group = c("X", "X", "X", "X", "Y", "Y", "Y", "Y"),
    value = c(10, 20, 15, 25, 12, 18, 22, 28)
)

plot_bar <- function(dt, category_col, value_col, facet_col = NA) {
  if (is.character(facet_col)) {
    ggplot(dt, aes(x = .data[[category_col]], y = .data[[value_col]], fill = .data[[category_col]])) +
      geom_col() +
      geom_text(aes(label = .data[[value_col]]), vjust = -0.5) +
      facet_wrap(as.formula(paste("~", facet_col))) +
      theme_minimal()
  } else {
    ggplot(dt, aes(x = .data[[category_col]], y = .data[[value_col]], fill = .data[[category_col]])) +
      geom_col() +
      geom_text(aes(label = .data[[value_col]]), vjust = -0.5) +
      theme_minimal()
  }
}

plot_bar(data, category_col = "category", value_col = "value")
plot_bar(data, category_col = "category", value_col = "value", facet_col = "group")
