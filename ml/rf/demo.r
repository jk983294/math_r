# install.packages("ranger")

n <- 10000L # number of observation
p <- 10L # number of predictor
X <- matrix(rnorm(n * p, 0, 1), nrow = n, ncol = p)
y <- apply(X[, 1:p], 1, sum) + rnorm(n)

dt <- data.frame(X, y)

train_idx <- caret::createDataPartition(y, p = 0.8, list = FALSE)
train_data <- dt[train_idx, ]
test_data <- dt[-train_idx, ]

rf_model <- ranger::ranger(
  y ~ .,                  # Formula: predict y using all predictors
  data = dt,
  num.trees = 10,        # Number of trees
  mtry = NULL,            # Number of variables to try at each split (default is p/3 for regression)
  importance = "impurity" # Measure variable importance
)

print(rf_model)
pred <- predict(rf_model, data = dt)
cor(pred$predictions, y)
(rmse <- sqrt(mean((y - pred$predictions)^2)))

ranger::importance(rf_model)
