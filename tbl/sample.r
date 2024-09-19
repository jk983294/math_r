library(data.table)

dt <- data.table(matrix(1:100, nrow = 50))
dt[sample(nrow(dt), 5), ]
