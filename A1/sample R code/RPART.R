# install the libaray if this is the first time you use it.
# install.packages("rpart") 
# install.packages("rpart.plot") 
library(rpart)
library(rpart.plot)
library(gmodels)

# load data, there are only 6 cotinuous attributes and one continuous DV (earnings).
# no missing values in this example
credit <- read.csv("D:\\RData\\credit.csv")

# take a look at few observations to make sure nothing wrong.
str(credit)

# take a look at the descriptive statistics
summary(credit)

#####################################################
# Split the dataset for training and testing: same as C50 example R code so far
#####################################################
# a random seed that helps you replicate results in the future
set.seed(123)

# The following commands use the sample() function to select 900 values at random
# out of the sequence of integers from 1 to 1000.
train_sample <- sample(1000, 900)

# we use the randomly selected 900 numbers as the training set, the other 100 examples as the test set
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

#####################################################
# Building a classifier on the training set by RPART: now is different
#####################################################
model_baseline <- rpart(factor(credit_train$default)~., data=credit_train)
summary(model_baseline) # too complicated to read
# plot your tree
rpart.plot(model_baseline, digits = 4) # results are complicated but still can be interpreted

# Let's calculate the confusion matrix on the training set
p0 <- predict(model_baseline, credit_train, type="class")
CrossTable(credit_train$default, p0, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))

# Predicting on the test set
p1 <- predict(model_baseline, credit_test, type="class")
CrossTable(credit_test$default, p1, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))

######################## Pre-Pruning ##################################
preprune1 <- rpart(factor(credit_train$default)~., data=credit_train, control = rpart.control(minsplit = 90)) # each no-leaf node must have at least 90 cases
rpart.plot(preprune1, digits = 4)

preprune2 <- rpart(factor(credit_train$default)~., data=credit_train, control = rpart.control(minbucket = 90)) # each leaf node must have at least 90 cases
#  If  only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
rpart.plot(preprune2, digits = 4)

preprune3 <- rpart(factor(credit_train$default)~., data=credit_train, control = rpart.control(maxdepth = 3)) # maximum level of tree
rpart.plot(preprune3, digits = 4)

preprune4 <- rpart(factor(credit_train$default)~., data=credit_train, cp = 0.001) # you directly change CP, the smaller, the larger tree
rpart.plot(preprune4, digits = 4)

######################## Post-Pruning ##################################
# RPART has an important complexity parameter CP, the default value is 0.01
#####################################################
printcp(model_baseline)

# cp: complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. Default is 0.1.
# nsplit: number of splits. RPART is a binary tree and thus nsplit+1 is the number of leafs
# rel error: relative training error. you can use plotcp() to plot and observe that when you increases nsplit, the training error always decreases
# xerror: cross-validation error, an error closer to test error and is an important performance metric for selecting cp.
# xstd: std error of cross-validation error
##########################################################################################################
# Use the first level (i.e. least nsplit) with minimum xerror. 
# The first level only kicks in when there are multiple level having same, minimum xerror. 
# This is the most common used method.
##########################################################################################################

# Prune the tree using the best cp mentioned above.
bestcp <- model_baseline$cptable[which.min(model_baseline$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(model_baseline, cp = bestcp)

# plot your tree
rpart.plot(tree.pruned, digits = 4)

# Predicting on the test set
p3 <- predict(tree.pruned, credit_test, type="class")
CrossTable(credit_test$default, p3, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))







