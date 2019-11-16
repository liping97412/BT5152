library(caret)
library(xgboost)
library(DMwR)
library(ROSE)

set.seed(5152)

data <- read.csv("data/application_v3.csv")
data$TARGET <- factor(data$TARGET)
levels(data$TARGET) <- c("OTHER", "DIFFICULTY")

train_idx <- createDataPartition(data$TARGET, p = 0.7, list = FALSE)
train <- data[train_idx, ]
test  <- data[-train_idx, ]

ctrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE, sampling = "down", allowParallel = TRUE)

model.down <- train(TARGET ~ ., data = train, method = "xgbTree", metric = "ROC", trControl = ctrl)

pred.down <- predict(model.down, test)

cm.down <- confusionMatrix(pred.down, test$TARGET)
