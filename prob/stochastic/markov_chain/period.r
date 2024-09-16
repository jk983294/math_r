source("~/github/math_r/prob/stochastic/sp_utils.r")

m <- rbind(
    c(0, 0.5, 0, 0, 0, 0.5),
    c(0.5, 0, 0.5, 0, 0, 0),
    c(0, 0.5, 0, 0.5, 0, 0),
    c(0, 0, 0.5, 0, 0.5, 0),
    c(0, 0, 0, 0.5, 0, 0.5),
    c(0.5, 0, 0, 0, 0.5, 0)
)

matrix_power(m, 11)
matrix_power(m, 12)

# mc with period d means we have d eigenvalues equally spaced around the unit circle
# and P^n doesn't converge
(eigens <- eigen(t(m)))
