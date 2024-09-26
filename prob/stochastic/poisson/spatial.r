lambda <- 100
square_area <- 1
trials <- 10000
sim_list <- numeric(trials)
for (i in 1:trials) {
    # given N points, those points spread evenly in square (0, 0) -> (1, 1)
    N <- rpois(1, lambda * square_area)
    x_points <- runif(N, 0, 1)
    y_points <- runif(N, 0, 1)
    # The circle C inside the square is centered at (0.7, 0.7) with radius r = 0.2
    ct <- sum(((x_points - 0.7)^2 + (y_points - 0.7)^2) <= 0.2^2)
    sim_list[i] <- ct # number of points in circle
}
mean(sim_list)
var(sim_list)

# Compare to theoretical mean and variance
lambda * pi * (0.2)^2
