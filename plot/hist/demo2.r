library(ggplot2)
library(tibble)

# 把频数少的小区间合并， 把频数过大的小区间拆分， 产生不等距的概率直方图
x <- rcauchy(100)
d <- tibble(x = x)
breaks <- c(-100, -50, -30,  -10, -5, -2, -1, 0, 1, 2, 5, 10, 30, 100)
ggplot(data=d, mapping=aes(x=x, y=..density..)) +
  geom_histogram(breaks=breaks) +
  geom_density(color="red", size=1)
