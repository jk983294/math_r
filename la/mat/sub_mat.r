set.seed(123)
mat <- matrix(sample(1:100, 25), nrow = 5, ncol = 5)
rownames(mat) <- paste0("Row", 1:5)
colnames(mat) <- paste0("Col", 1:5)

# select single
mat[1, ]
mat["Row1", ]
mat[, 2]
mat[, "Col2"]

# select multi
mat[1:2, ]
mat[c("Row1", "Row2"), ]
mat[, 1:2]
mat[, c("Col1", "Col2")]

# negative select
mat[c(-1, -2), ]
mat[, c(-1, -2)]
