library(caret)
library(C50)

setwd("d:/RData/") # replace with your own working directory
credit <- read.csv("credit.csv")
credit$default <- factor(credit$default)

# if you like to shuffle dataset, the following are the sample codes
# s <- sample(1:nrow(credit))
# credit <- credit[s,]

train_data <- credit[1:900,]
test_data <- credit[901:1000,]
#summary(train_data)
#summary(test_data)

# fill in the following function for grid search, set .model = "tree", set .winnow = FALSE, tune ".trials" from 5 to 35
tr_grid <- expand.grid()

# fill in the following function for cross validation
# first, read the manual by help(trainControl) to find out how to use different kinds of cross validation
# second, you are required to use 
# 2-fold cross validation for model_cv1
# 10-fold cross validation for model_cv2
# 10-fold cross validation with repeats=5 for model_cv3
# 10-fold cross validation with selectionFunction = "oneSE" for model_cv4, read the manual to understand the meaning of oneSE method
# 10-fold cross validation with selectionFunction = "tolerance" for model_cv5, read the manual to understand the meaning of tolerance method
model_cv1 <-  train(default ~ ., data=train_data, method='C5.0', trControl=trainControl(), tuneGrid = tr_grid)
model_cv2 <- train(default ~ ., data=train_data, method='C5.0', trControl=trainControl(), tuneGrid = tr_grid)
model_cv3 <- train(default ~ ., data=train_data, method='C5.0', trControl=trainControl(), tuneGrid = tr_grid)
model_cv4 <- train(default ~ ., data=train_data, method='C5.0', trControl=trainControl(), tuneGrid = tr_grid)
model_cv5 <- train(default ~ ., data=train_data, method='C5.0', trControl=trainControl(), tuneGrid = tr_grid)

# after you build the model, you can use run "model_cv1" for example to find the optimal parameter
# pay attention to the optimal parameter of .trials of 5 cases
# also check the accuracy on the test set by completing the following commands

print(confusionMatrix(,)
print(confusionMatrix(,)
print(confusionMatrix(,)
print(confusionMatrix(,)
print(confusionMatrix(,)

# The following is not graded in A2, it is for you to explore further.
# After you complete this exercise once, you can repeat line #28-32 again several times, 
# check the optimal parameters and the number of trials. What is the optimal value of .trials? Which cross-validation method you feel is the best?
# If you like, you can also practice writing a loop to repeat 10 or more times, calculate the mean and variance of accuracy to find out which method performs the best on this dataset.