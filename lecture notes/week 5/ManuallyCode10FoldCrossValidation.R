### Import the library for C5.0 Classification Tree and load a default dataset for practicing
library(C50)
#Load library with larger datasets and import Breast Cancer dataset
library(mlbench)
data("BreastCancer")

#Remove index column, which is not useful 
BreastCancer <- BreastCancer[-1]

#Again divide data into training and testing
indexes <- sample(1:699,100)

train_data <- BreastCancer[indexes,]
test_data <- BreastCancer[-indexes,]

##########################
### Appendix: Manual 10-fold cross-validation
##########################
library(caret) # for createFolds
library(vcd) # for Kappa()
# Generate 10-fold partition
folds <- createFolds(BreastCancer$Class, k = 10)

#Run cross validation
cv_results <- lapply(folds, function(x){
  #this part is performed for each entry of the folds list
  fold_test <- BreastCancer[x,]
  fold_train <- BreastCancer[-x,]
  fold_model <- C5.0(Class ~ ., data = fold_train)
  fold_pred <- predict(fold_model, fold_test)
  fold_actual <- fold_test$Class
  fold_kappa <- Kappa(table(fold_actual, fold_pred))
  return(fold_kappa$Unweighted[1])
})

str(cv_results)
#Find average Kappa score
mean(unlist(cv_results))



