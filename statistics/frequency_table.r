data1 <- sample(1:6, 100L, replace = TRUE)
data2 <- sample(1:6, 100L, replace = TRUE)
dt <- cbind(x = data1, y = data2)

# one way table
tbl1 <- table(data1)
prop.table(tbl1) # proportions

# two way table
tbl2 <- xtabs(~ x + y, data = dt)
tbl2 # frequencies

# 边际频数和比例
margin.table(tbl2, 1) # row sums
margin.table(tbl2, 2) # column sums
prop.table(tbl2) # cell proportions
prop.table(tbl2, 1) # row proportions
prop.table(tbl2, 2) # column proportions
addmargins(tbl2) # add row and column sums to table

# more complex tables
addmargins(prop.table(tbl2))
addmargins(prop.table(tbl2, 1), 2)
addmargins(prop.table(tbl2, 2), 1)
