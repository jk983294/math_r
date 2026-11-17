##### demo: norm-proportional column sampling for random matrix multiplication
# Random matrix multiplication approximates a large product with a small,
# randomly sampled sketch. The key choice is HOW to sample. If we sample
# columns proportional to their squared l2 norm (their "importance"), a single
# heavy column can never be wrongly ignored -- so the sketch is accurate with
# far fewer samples than uniform sampling would need.
#
# Section 1: sketch ONE matrix A by sampling its columns with
#     p_j = ||A_{:,j}||^2 / ||A||_F^2
#   Rescale each sampled column by 1/sqrt(c p_j). With C = sketch (m x c),
#     E[ C C^T ] = A A^T          (the row Gram / column space is preserved)
#   (Norm-proportional sampling => low variance even when norms are skewed.)
# Section 2: approximate a FULL product A B by sampling a shared index set
#   k with weight ||A_{:,k}|| * ||B_{k,:}||, building C (column sketch of A)
#   and R (row sketch of B) so that E[ C R ] = A B (m x p). Classical
#   Drineas/Kannan/Mahoney randomized matrix multiplication.

set.seed(42)

##### Section 1 -- column sketch preserving A A^T
# matrix with very skewed column norms: uniform sampling would fail badly
A <- cbind(10 * matrix(rnorm(20), 10, 2),    # two large-norm columns
            0.3 * matrix(rnorm(20), 10, 2))   # two small-norm columns
col_norms <- sqrt(colSums(A^2))
p <- col_norms^2 / sum(col_norms^2)         # norm-proportional probs
p; sum(p)                                    # sums to 1
true_gram <- A %*% t(A)                      # target we approximate in Sec 1

# build the sketch: sample c columns with prob p, rescale by sqrt(c p_j).
# sweep(M, 2, v, '/') divides COLUMN j by v[j] (plain 'M / v' would recycle
# the vector down each column and mis-scale when nrow != c).
sample_cols <- function(A, c) {
  p <- colSums(A^2) / sum(colSums(A^2))
  idx <- sample(ncol(A), c, replace = TRUE, prob = p)
  sweep(A[, idx, drop = FALSE], 2, sqrt(c * p[idx]), '/')
}

C <- sample_cols(A, 3)
round(C %*% t(C), 2)      # one realization -- unbiased estimator of A A^T
round(true_gram, 2)

# average relative error vs number of sampled columns (many trials)
err_np <- function(c) mean(replicate(200, {
  C <- sample_cols(A, c)
  norm(C %*% t(C) - true_gram, 'F') / norm(true_gram, 'F')
}))
cat('norm-proportional column sampling, A A^T error:\n')
for (c in c(1, 2, 4, 8, 16)) cat(sprintf('  c=%2d  rel err ~ %.3f\n', c, err_np(c)))

# contrast: UNIFORM sampling (ignores column norms) -- same unbiasedness,
# but much larger variance when norms are skewed
err_unif <- function(c) mean(replicate(200, {
  idx <- sample(ncol(A), c, replace = TRUE)
  C <- sweep(A[, idx, drop = FALSE], 2, sqrt(c / ncol(A)), '/')
  norm(C %*% t(C) - true_gram, 'F') / norm(true_gram, 'F')
}))
cat('\nuniform vs norm-proportional (same c):\n')
for (c in c(2, 4, 8)) cat(sprintf('  c=%2d  uniform=%.3f  norm-prop=%.3f\n',
                                 c, err_unif(c), err_np(c)))

##### Section 2 -- randomized matrix multiplication A B
# sample a shared index k with weight ||A_{:,k}|| * ||B_{k,:}||;
# form C = [A_{:,k}/sqrt(c p_k)] and R = [B_{k,:}/sqrt(c p_k)];  then E[C R] = A B.
set.seed(7)
A2 <- matrix(rnorm(15), 5, 3)     # 5 x 3
B2 <- matrix(rnorm(12), 3, 4)     # 3 x 4
true_prod <- A2 %*% B2

sketch_prod <- function(A, B, c) {
  p <- sqrt(colSums(A^2)) * sqrt(rowSums(B^2))
  p <- p / sum(p)
  idx <- sample(ncol(A), c, replace = TRUE, prob = p)
  v <- sqrt(c * p[idx])
  C <- sweep(A[, idx, drop = FALSE], 2, v, '/')   # column j / sqrt(c p_j)
  R <- sweep(B[idx, , drop = FALSE], 1, v, '/')   # row i    / sqrt(c p_i)
  C %*% R
}

cat('\nrandomized matrix multiplication A B (expectation -> A B):\n')
cat('  one sketch, c=3, rounded:\n'); print(round(sketch_prod(A2, B2, 3), 3))
cat('  true product rounded:\n');            print(round(true_prod, 3))

err_prod <- function(c) mean(replicate(200, {
  norm(sketch_prod(A2, B2, c) - true_prod, 'F') / norm(true_prod, 'F')
}))
cat('\nrelative error vs sampled indices c:\n')
for (c in c(1, 2, 3, 6, 12)) cat(sprintf('  c=%2d  rel err ~ %.3f\n', c, err_prod(c)))
