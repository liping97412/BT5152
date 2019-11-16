library(xgboost) # for xgboost
setwd("D:/RData/") 
train_data <- read.csv("a3_train.csv")
test_data <- read.csv("a3_test.csv")

# the follow steps are unique to xgboost, we cannot use the data frame object as the input
# also, the lable should be a number even it is binary
dtrain <- xgb.DMatrix(as.matrix(train_data[-16]), label=train_data$y)
dtest <- xgb.DMatrix(as.matrix(test_data[-16]))

# main training step
# help(xgboost)
xgb <- xgboost(data = dtrain,
               eta = 0.3,
               max_depth = 3, 
               nround=100, 
               gamma=1,
               subsample = 1,
               colsample_bytree = 1,
               min_child_weight = 1,
               objective = "binary:logistic",
               eval_metric = "error"
)

# Prediction step
pred <- predict(xgb, dtest)
# the output is predicted probability and we need to convert it to binary
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

# cross-validation, you can observe the training error decreases but test error is U-shape
cv.results <- xgb.cv(data = dtrain, nfold = 10,
               eta = 0.03,
               max_depth = 2, 
               nround=500, 
               gamma=0,
               subsample = 1,
               colsample_bytree = 0.8,
               min_child_weight = 1,
               objective = "binary:logistic",
               eval_metric = "error"
)

# Importance of Features
# help(xgb.importance)
importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
help(xgb.importance)
xgb.plot.importance(importance_matrix = importance_matrix)

# Plot the tree, but not easy to visualize when there are many trees
# xgb.plot.tree(model = xgb)

# Customize Objective Function: but use it with caution, you need to understand the theory
# especially the meaning of grad and hess
loglossobj <- function(preds, dtrain) {
  # dtrain is the internal format of the training data
  # We extract the labels from the training data
  labels <- getinfo(dtrain, "label")
  # We compute the 1st and 2nd gradient, as grad and hess
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  # Return the result as a list
  return(list(grad = grad, hess = hess))
}

xgb_cust_obj <- xgboost(data = dtrain, 
               eta = 0.3,
               max_depth = 3, 
               nround=100, 
               gamma=1,
               subsample = 1,
               colsample_bytree = 1,
               min_child_weight = 1,
               objective = loglossobj, # this is the important new command
               eval_metric = "error" # use this metric
)

# according to the documentation, this should be equivalent to using objective = "binary:logistic".
# this is also our baseline code, but results are slightly different.

