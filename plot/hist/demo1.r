library(ggplot2)
library(tibble)

polt_hist <- function(x, n_bins = 10L, use_density_hight = FALSE){
  if (use_density_hight) {
    ggplot2::ggplot(data=tibble::tibble(x=x), mapping=ggplot2::aes(x=x, y=..density..)) +
      ggplot2::geom_histogram(bins=n_bins) +
      ggplot2::geom_density(color="red", size=1)
  } else {
    ggplot2::ggplot(data=tibble::tibble(x=x), mapping=ggplot2::aes(x=x)) +
      ggplot2::geom_histogram(bins=n_bins) +
      ggplot2::geom_density(color="red", size=1)
  }
}

x <- rnorm(100, 0, 1)
polt_hist(x, 20)
polt_hist(x, 20, TRUE)
