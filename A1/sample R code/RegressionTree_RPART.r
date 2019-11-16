#Example - estimating the quality of wines with regression trees and model trees

# install.packages("rpart")
library("rpart")

# load data
wine <- read.csv("d://RData//whitewines.csv")

# take a look at the data
str(wine)
summary(wine)

# split dataset into training set and test set
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# the main step to use RPART package for regression tree
m.rpart <- rpart(quality ~ ., data = wine_train)

# check the results
m.rpart
# Right after each tree's rule, the value represents the number of cases covered.
# Nodes indicated by * are terminal or leaf nodes, which means that they result in a prediction (listed here as yval).

# A more detailed summary of the tree's fit, including the mean squared error for each of the nodes and an overall measure of feature importance, 
# can be obtained using the following command
summary(m.rpart)

# The tree by RPART is quite messy, a better visualization
install.packages("rpart.plot")
library(rpart.plot)

# a nicer plot
rpart.plot(m.rpart, digits = 3)

# Plot function supports many controls
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
# The fallen.leaves parameter forces the leaf nodes to be aligned at the bottom of the plot. 
# Type and extra parameters affect the way the decisions and nodes are labeled.
# Type of plot. Possible values: 0 Draw a split label at each split and a node label at each leaf.
# 1 Label all nodes, not just leaves. Similar to text.rpart's all=TRUE.
# 2 Default. Like 1 but draw the split labels below the node labels. Similar to the plots in the CART book.
# 3 Draw separate split labels for the left and right directions.
# 4 Like 3 but label all nodes, not just leaves. Similar to text.rpart's fancy=TRUE. See also clip.right.labs.
# extra = 101 part is an important argument about displaying extra information at the nodes. see the source for the full list
# source: https://www.rdocumentation.org/packages/rpart.plot/versions/2.1.2/topics/rpart.plot

# Make a prediction
p.rpart <- predict(m.rpart, wine_test)
# No confusion matrix for "regression", we check the following performance metrics
cor(p.rpart, wine_test$quality)

# MAE
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)