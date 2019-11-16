library(e1071)
library(caret)

train <- read.csv("data/A3_train.csv", colClasses = append(rep(c("numeric"), times = 15), "factor"))

cols <- c('x4', 'x8', 'y')

train_t <- train[1:1500, cols]
train_v <- train[1501:2000, cols]

model.linear <- svm(y ~ ., data=train_t, kernel = 'linear', cost = 1)
plot(model.linear, train_t)

pred.linear = predict(model.linear, train_v)
confusionMatrix(pred.linear, train_v$y)
