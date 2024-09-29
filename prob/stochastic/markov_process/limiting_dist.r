library(expm)

generator_matrix <- matrix(c(
    -2, 1, 1,
    1 / 2, -1, 1 / 2,
    0, 1 / 3, -1 / 3
), nrow = 3, byrow = T)

t <- 100
(dist <- expm::expm(t * generator_matrix))
