library(data.table)
library(fst)

file <- file.path("/tmp", sprintf("%d.csv", 20230703))

dt <- data.table(x = c("a", "b", "c", "b"), y = c(2, 1, 3, 4), z = c(1, 3, 2, 2))
fwrite(dt, "/tmp/test_dt.csv")

dt1 <- fread("/tmp/test_dt.csv")

write.fst(dt, "/tmp/dataset_dt.fst")
dt1 <- setDT(read.fst("/tmp/dataset_dt.fst"))
