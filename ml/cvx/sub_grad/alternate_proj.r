alternating_projections <- function(proj1, proj2, x0, tol = 1e-6, maxiter = 1000) {
  x_old <- x0
  iter <- 0
  converged <- FALSE
  
  while (iter < maxiter) {
    iter <- iter + 1
    x1 <- proj1(x_old)       # Project onto first set (C)
    x_new <- proj2(x1)        # Project onto second set (D)
    dx <- sqrt(sum((x_new - x_old)^2))  # Euclidean distance
    
    if (dx < tol) {
      converged <- TRUE
      break
    }
    x_old <- x_new
  }
  
  list(point = x_new, iter = iter, converged = converged)
}


proj_line <- function(p) c(p[1], 0)  # Project to x-axis

proj_disk <- function(p) {
  r <- sqrt(sum(p^2))
  if (r <= 1) p else p / r  # Project to unit disk
}

result <- alternating_projections(
  proj1 = proj_line, 
  proj2 = proj_disk, 
  x0 = c(2, 0)
)
print(result$point)  # Output: c(1, 0)
