x <- c(1., 2, 3, 4, 5, NA)
y <- c(6., 7, 8, 9, 10, NA)

# null hypothesis: the means are equal
result <- t.test(x, y)

print(result)

# mean difference test that does not assume a normal distribution of the data.
result <- wilcox.test(x, y, paired = TRUE)
print(result)
