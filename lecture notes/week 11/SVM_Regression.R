# Reference: 
library(e1071)
data <- read.csv("SVMRegression.csv")

############### OLS Baseline ###############
plot(data, pch=16)

# Create a linear regression model
model <- lm(Y ~ X, data)

# Add the fitted line
abline(model)

# make a prediction for each X
predictedY <- predict(model, data)

# add the predictions from OLS
points(data$X, predictedY, col = "blue", pch=4)

############### SVM Regression ###############
# help(svm)
model <- svm(Y ~ X , data) # withotu epsilon, you can run SVM regression, the default value is 0.1 and you can tune to change it
predictedYSVM <- predict(model, data)

# Add red dot are the predicted vcalue of SVM regression
points(data$X, predictedYSVM, col = "red", pch=4)

# perform a grid search on C and epsilon together
tuneResult <- tune(svm, Y ~ X,  data = data,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))) # epsilon from 0 to 1 with step 0.1
print(tuneResult)
# Draw the tuning graph
# I comment the following graph so you can still see the predicted value on the orignal plot
# plot(tuneResult) 

# using the best model 
tunedModel <- tuneResult$best.model
tunedModelYSVM2 <- predict(tunedModel, data) 
# Add the predictions
points(data$X, tunedModelYSVM2, col = "green", pch=4)

# Last remark, in this example, there is no testing dataset, so our best model may overfit although there are 10-fold cross validation during tuning.