# Waiting time paradox
# The surprising result is that the inter-arrival time of the buses before and after Lisa’s arrival is about 20 minutes.
# And yet the expected inter-arrival time for buses is 10 minutes!

my_arrive_time <- 50
lambda <- 1 / 10
trials <- 10000
sim_list <- numeric(trials)
for (i in 1:trials) {
    N <- rpois(1, 300 * lambda)
    arrivals <- sort(runif(N, 0, 300))
    wait <- arrivals[arrivals > my_arrive_time][1] - my_arrive_time
    sim_list[i] <- wait
}
mean(sim_list)
