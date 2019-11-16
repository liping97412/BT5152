# load library of NB
install.packages("e1071")
library(e1071)

# this small dataset is one of the most famous built-in dataset for demo purpose
data(iris)
summary(iris)

# re-order/shuffle rows
iris <- iris[sample(1:nrow(iris)), ]

# split the dataset into training set and test test
iris_train <- iris[1:100, ]
iris_test <- iris[101:150, ]
summary(iris_test)

# the following function is a more typical function call than KNN
# we typically have the the first arugment as an equation that tells the program which is the class variable and also features to be included
# the 2nd required input is the name of your dataframe object
trained_model <- naiveBayes(Species ~ ., data = iris_train) 
# the default Laplace is 0
# for numeric variables, the default is to use normal distribution for fitting your features.

# the following two are generic commands that can tell you some information about your predictive model
summary(trained_model)
print(trained_model)

# Prediction stage: the following is also a generic and common function for prediction
# first input is the model you built in the training stage, the second one is the test dataset, the third is about predicting class
# type="raw" will output the predicted probabilities
predicted_class <- predict(trained_model, iris_test[,-5], type="class")
# take a look at the prediction results
summary(predicted_class)
print(predicted_class)

# first method to see the confusion matrix
install.packages("descr") # for cross table function
library("descr")
CrossTable(x = iris_test[,5], y = predicted_class, prop.chisq = FALSE)

# a more common command to see the confusion matrix
table(predicted_class, iris_test[,5])

# try the version with laplace=1
# in the example i tried, the results are the same
trained_model_v2 <- naiveBayes(Species ~ ., data = iris_train, laplace=1) 
predicted_class_v2 <- predict(trained_model_v2, iris_test[,-5], type="class")
CrossTable(x = iris_test[,5], y = predicted_class_v2)

