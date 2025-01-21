fracDiff <- function(series, d, window_size = 100) {
  # Calculate weights for fractional differentiation
  weights <- numeric(window_size)
  weights[1] <- 1.0
  for (k in 2:window_size) {
    weights[k] <- -weights[k - 1] * (d - k + 2) / (k - 1)
  }
  weights <- rev(weights)  # Reverse weights for convolution
  
  # Apply fractional differentiation
  diff_series <- numeric(length(series) - window_size)
  for (t in (window_size + 1):length(series)) {
    diff_series[t - window_size] <- sum(weights * series[(t - window_size):(t - 1)])
  }
  
  # Return the differentiated series as a time series object
  return(ts(diff_series, start = time(series)[window_size + 1], frequency = frequency(series)))
}

series <- ts(rnorm(1000))  # Random time series
d <- 0.5  # Fractional order
diff_series <- fracDiff(series, d)

# Plot the original and fractionally differentiated series
plot(series, main = "Original Series", col = "blue")
plot(diff_series, main = "Fractionally Differentiated Series", col = "red")
tseries::adf.test(diff_series) # test the stationarity
