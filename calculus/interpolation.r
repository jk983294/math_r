x <- c(1, 2, 4, 5)
y <- c(2, 4, 6, 8)

# Perform linear interpolation
interpolated_values <- approx(x, y, xout = c(1.5, 3, 4.5))
print(interpolated_values) # Print the interpolated values

# record function first, then evaluate at given points
interpolate_func <- approxfun(x, y)
interpolated_values <- interpolate_func(c(1.5, 3, 4.5))
print(interpolated_values)

# spline interpolation
spline_result <- spline(x, y, n = 100)
print(spline_result)
plot_interpolation <- function(x, y, result) {
    plot(x, y, col = "blue", pch = 16, main = "Spline Interpolation", xlab = "x", ylab = "y")
    lines(result, col = "red")
    legend("topleft", legend = c("Original Points", "Interpolated Curve"), col = c("blue", "red"), pch = c(16, -1), lty = c(0, 1))
}
plot_interpolation(x, y, spline_result)
