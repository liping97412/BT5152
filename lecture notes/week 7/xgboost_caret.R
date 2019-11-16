library(caret)
library(xgboost) # for xgboost

# load data, A3
setwd("D:/RData/") 
train_data <- read.csv("a3_train.csv")
test_data <- read.csv("a3_test.csv")
train_data$y<-factor(train_data$y)


#################################
### Tuning XGBoost with Caret ###
#################################
modelLookup("xgbTree")

# Add cross-validation, and Caret also automatically try to tune your parameters
ctrl <- trainControl(method = "cv", number = 2)
m <- train(y ~ ., data = train_data, method = "xgbTree", trControl = ctrl)
m

# Add your own customized grid search for performance tuning
grid <- expand.grid(.nrounds=c(40,50,60),.eta=c(0.2,0.3,0.4),.gamma=c(0,1),.max_depth=c(2,3,4),.colsample_bytree=c(0.8),.subsample=c(1),.min_child_weight=c(1))
starttime<-Sys.time()
m <- train(y ~ ., data = train_data, method = "xgbTree", trControl = ctrl, tuneGrid = grid)
endtime<-Sys.time()
(endtime-starttime)
m
