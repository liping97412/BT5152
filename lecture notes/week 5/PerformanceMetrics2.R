#Load library with larger datasets and import Breast Cancer dataset
#install.packages("mlbench")
library(mlbench)
data("BreastCancer")

#Remove index column, which is not useful 
BreastCancer <- BreastCancer[-1]

#Again divide data into training and testing
indexes <- sample(1:699,100)

train_data <- BreastCancer[indexes,]
test_data <- BreastCancer[-indexes,]

model <-C5.0(train_data[-10], train_data$Class)
summary(model)

predictions_class <- predict(model, test_data[-10], type = 'class')
predictions_prob <- as.data.frame(predict(model, test_data[-10], type = 'prob'))

# Combine outputs, sometimes you need to create the following objects for further analysis in R or in other software
results <- test_data[10]
str(results)
results$predicted_type <- predictions_class
results$probs_benign <- predictions_prob$benign
results$probs_malignant <- predictions_prob$malignant
results # now we can have a table for further analysis
summary(results)

#############################################################
# 3 mthods to create confusion matrix with performance matrix
############################################################
# Method 1: traditional method, oversimplified, not good enough
table(results$Class, results$predicted_type)

# the downside is you need to manually calculate performance metrics in this low-tech case
Accuracy <- (362 + 189) / (362+189+12+36)
Error_rate <- (12 + 36) / (362+189+12+36)
Precisiion <- (362)/(362+36)

# Method 2: gmodels's CrossTable function that we have seen
library(gmodels)
CrossTable(results$Class, results$predicted_type)
# you can trun off the values that you dont need, two more useful commands below
# help(CrossTable)
# CrossTable(results$Class, results$predicted_type, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE)
# CrossTable(results$Class, results$predicted_type, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

# Method 3: The powerful Caret Package
# install.packages("caret", dependencies = TRUE)
library('caret')
confusionMatrix(results$predicted_type, results$Class, positive = "malignant")

##################### End of 3 Confusion Matrices #####################################

#####################################################################################
## Similar to other cases, there are more than one way in R to calculate Kappa, one other method is to use vcd package when you need each function
## install.packages("vcd")
#####################################################################################
library(vcd)
Kappa(table(results$Class, results$predicted_type))
sensitivity(results$predicted_type, results$Class, positive = "malignant")
specificity(results$predicted_type, results$Class, positive = "malignant")
prec <- precision(results$predicted_type, results$Class, positive = "malignant")
rec <- recall(results$predicted_type, results$Class, positive = "malignant")

#F1-score/ F-measure
(2 * prec * rec)/(prec + rec)
F_meas(results$predicted_type, results$Class, positive = "malignant")

###########
### ROC ###
########### 
# install.packages("ROCR")
library(ROCR)

pred <- prediction(predictions = results$probs_malignant, labels =  results$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Plot ROC curve
plot(perf)
#Plot dashed diagonal line 
abline(0,1, lwd = 2, lty = 2)

#Calculate Area Under the Curve
perf.auc <- performance(pred, measure = "auc")
perf.auc@y.values

