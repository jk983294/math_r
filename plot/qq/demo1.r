library(ggplot2)
library(tibble)

polt_qq <- function(x){
  ggplot2::ggplot(data=tibble::tibble(x=x), mapping=ggplot2::aes(sample=x)) +
    ggplot2::stat_qq() +
    ggplot2::geom_qq_line(color="red")
}

x <- rnorm(100, 0, 1)
x <- rlnorm(100, 0, 1) # log-normal
x <- -rexp(100) # negative exp dist
x <- rt(100, 2) # t dist with heavy tail
polt_qq(x)

