vec1 <- c(576, 580, 653, 635, 555, 575, 558, 661, 545, 578, 651, 572, 666, 605, 594)
vec2 <- c(3.39, 3.07, 3.12, 3.3, 3., 2.74, 2.81, 3.43, 2.76, 3.03, 3.36, 2.88, 3.44, 3.13, 2.96)
law <- cbind(x = vec1, y = vec2)

cor(law)
theta <- function(data, indices) {
    cor(data[indices, 1], data[indices, 2])
}
results <- boot::boot(law, theta, 1000L)
boot::boot.ci(results, type = "bca")
