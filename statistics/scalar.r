x <- c(12, 7, 3, 4.2, 18, 2, 54, -21, 8, -5)
mean(x) # 8.22
mean(x, trim = 0.3) # 5.55 截尾平均数, drop 30% max and min values, then calculate the mean
mean(x, na.rm = TRUE) # 8.22 drop NA values
sd(x) # 19.20057 standard deviation
var(x) # 368.6618 方差
median(x) # 5.6
mad(x) # 7.413 绝对中位差(median absolute deviation)
sum(x) # 82.2
max(x) # 54/home/kun/github/math/r/learning/statistics/kernel_density.r
min(x) # -21
range(x) # 求值域 [-21, 54]
IQR(x) # describes the middle 50% of values, difference between Q3 and Q1
quantile(x, 0.3) # 2.7
quantile(x, c(0.3, 0.6)) # [2.7, 7.4]
diff(x) # 差分 [-5.0  -4.0   1.2  13.8 -16.0  52.0 -75.0  29.0 -13.0]
diff(x, lag = 2) # [-9.0  -2.8  15.0  -2.2  36.0 -23.0 -46.0  16.0]
scale(x) # scale to Gaussian dist, mean = 0, sd = 1
scale(x) * 2 + 3 # scale to Gaussian dist, mean = 3, sd = 2
summary(x) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.

# mode is value that has highest number of occurrences in a set of data
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
charv <- c("o", "it", "the", "it", "it")
getmode(charv) # 'it'
