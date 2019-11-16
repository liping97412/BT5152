# install the libaray if this is the first time you use it.
# install.packages("C50") 
# install.packages("gmodels")
library(C50)

# load data, example from the textbook
credit <- read.csv("D:\\RData\\credit.csv")

# take a look at few observations to make sure nothing wrong.
str(credit)

# take a look at the descriptive statistics
summary(credit)

# a random seed that helps you replicate results in the future
set.seed(123)

# The following commands use the sample() function to select 900 values at random
# out of the sequence of integers from 1 to 1000.
train_sample <- sample(1000, 900)

# we use the randomly selected 900 numbers as the training set, the other 100 examples as the test set
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

# make sure two sets have similar distribution in labels, numbers will be different from the textbook due to sampling
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


#####################################################
# Building a decision tree classifier by library C50
#####################################################
credit_model <- C5.0(credit_train[-17], factor(credit_train$default)) #-17 means the 5th column is not included since it is a label
# textbook uses the above command, similar to many other R packages, you can use the more common input format below
# credit_model <- C5.0(credit_train$default ~ ., data = train_data)

# you will see a very complicated tree with quite high accuracy
summary(credit_model)
# for each line of results, for example "installment_plan = none: 1 (286/24)" means when installment_plan = none, the class is "1", there are 286 cases and 24 mistakes

# make prediction on the test set by this complicated model
credit_pred <- predict(credit_model, credit_test)

# use CrossTable function to check the Confusion Matrix, likely you should observe the error rates are lower than the training set error rate
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
CrossTable(credit_test$default, credit_pred, dnn = c('actual default', 'predicted default'))
# the three controls= FALSE means do not output those statistics. You can turn on the prop.c and prop.r by deleting those, and it won't be difficult to understand its meaning.
# prop.chisq = FALSE is a statistics rarely used and I typically keep that statistics off
# more details on https://www.rdocumentation.org/packages/gmodels/versions/2.16.2/topics/CrossTable

#####################################################
# C50 has several additional features: simple boosting, https://cran.r-project.org/web/packages/C50/C50.pdf
#####################################################
credit_boost10 <- C5.0(credit_train[-17], factor(credit_train$default), trials = 10)
# you should observe the training set accuracy improves
summary(credit_boost10)
# now try prediction
credit_boost_pred10 <- predict(credit_boost10, credit_test)
# test set accuracy should be lower than training set but higher than the baseline test set because boosting typicaly helps
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

###########################################################
# C50's additional features: pre-pruning by minCases
# minCases: an integer for the smallest number of samples that must be put in at least two of the splits
# just a value of 10 cases can reduce the number of trees a lot in this case
###########################################################
credit_pruned <- C5.0(credit_train[-17], factor(credit_train$default), control = C5.0Control(minCases = 10))
summary(credit_pruned)
# how is prediction now?
credit_boost_pred_pruned <- predict(credit_pruned, credit_test)
CrossTable(credit_test$default, credit_boost_pred_pruned, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))
# in my trial, one pre-pruned tree works as well as the boosting by 10 complicated trees...

###########################################################
# C50's additional features: post-pruning? 
# C50 has a control called "noGlobalPruning" but sometimes it does not work well.
###########################################################

