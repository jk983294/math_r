# generate a random 0-1 sequence with no adjacent 1's
# count the number of 1's and take the average
# k:   sequence length
# n:   number of steps to run the chain

adjacent <- function(k, n) {
    init <- rep(0, k) # start at sequence of all 0's
    res <- numeric(n)
    new_seq <- c(2, init, 2) # pad sequence at the ends
    for (i in 1:n) {
        index <- 1 + sample(1:k, 1) # choose one random bit to process
        new_bit <- 0 + !new_seq[index] # flip the bit
        if (new_bit == 0) {
            new_seq[index] <- 0
            res[i] <- sum(new_seq)
            next
        } else {
            if (new_seq[index - 1] == 1 | new_seq[index + 1] == 1) {
                res[i] <- sum(new_seq)
                next
            } else {
                new_seq[index] <- 1
            }
            res[i] <- sum(new_seq)
        }
    }
    return(res)
}

k <- 100L
results <- adjacent(k, 100000)
mean(results) - 4 # 4 is padding of two side
