library(data.table)

dt <- data.table(x = c("a", "b", "c", "b"), y = c(2, 1, 3, 4), z = c(1, 3, 2, 2))

(dt[order(x, -z)])
# refer to order.r for more example