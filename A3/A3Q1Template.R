library(rpart)
library(ggplot2) # for the diamonds dataset

set.seed(1234)

data(diamonds)

# The full dataset is too big
sample_size = 3000
diamonds_subset <- diamonds[sample(1:nrow(diamonds), sample_size), ]

train_ids <- sample(1:sample_size, 0.8*sample_size)
train_data <- diamonds_subset[train_ids, ]
test_data <- diamonds_subset[-train_ids, ]

# Train a single tree model as our base model, to which we can compare with
# our random forest implementation later
model_rpart <- rpart(price ~., train_data)
pred_rpart <- predict(model_rpart, test_data)

rmse <- function(predicted, actual) {
  return(sqrt(mean((predicted - actual)^2)))
}

cat('Single tree model RMSE:', rmse(pred_rpart, test_data$price), '\n')

####################################
####    Manual Random Forest     ###
####################################

train_random_forest <- function(n_trees, n_features,
                                training_data, target_col_name){
  models <- lapply(1:n_trees, function(i) {
    # bootstrapping
    n_samples <- nrow(training_data)
    sample_row_ids <- sample(1:n_samples, replace=TRUE)
    new_training_data <- training_data[sample_row_ids, ]

    ### START CODE HERE ### (≈ 5 lines)
    # Subset n_features columns.
    # Be careful to prevent target column from being sampled,
    # but make sure it's eventually present in new_training_data
    ### END CODE HERE ###

    formula <- as.formula(paste(target_col_name, '~.'))
    new_model <- rpart(formula, data=new_training_data)

    ### START CODE HERE ### (≈ 2 lines)
    # post-prune the rpart model & return it
    return(new_model)
    ### END CODE HERE ###
  })
  return(models)
}

predict_random_forest <- function(models, test_data) {
  preds <- sapply(models, function(model) {
    return(predict(model, test_data))
  })
  return(rowSums(preds) / length(models))
}

models_rf <- train_random_forest(50, 4, train_data, 'price')
pred_rf <- predict_random_forest(models_rf, test_data)
cat('Random Forest RMSE:', rmse(pred_rf, test_data$price), '\n')
