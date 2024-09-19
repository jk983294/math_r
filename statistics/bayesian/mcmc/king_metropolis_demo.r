# the king should visit each island in proportion to its population size
# All he needs to know at any point in time is the population of the current island and the population of the proposal island.
num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
for ( i in 1:num_weeks ) {
    positions[i] <- current # record current position

    # flip coin to generate proposal
    proposal <- current + sample( c(-1,1) , size=1 )
    # now make sure he loops around the archipelago
    if ( proposal < 1 ) proposal <- 10
    if ( proposal > 10 ) proposal <- 1

    # move?
    prob_move <- proposal / current
    current <- ifelse( runif(1) < prob_move , proposal , current )
}

table(positions) / num_weeks
