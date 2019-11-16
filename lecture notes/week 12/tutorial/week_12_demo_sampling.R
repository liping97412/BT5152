# Modified from https://shiring.github.io/machine_learning/2017/04/02/unbalanced
# Additional Reference: https://www.marcoaltini.com/blog/dealing-with-imbalanced-data-undersampling-oversampling-and-proper-cross-validation
library(tidyverse)
library(caret)

data("BreastCancer", package = "mlbench")
#complete.cases Return a logical vector indicating which cases are complete, i.e., have no missing values.
bc_data <- BreastCancer[complete.cases(BreastCancer),]
summary(bc_data$Class)

# Reinitialise seed after each model is trained
set.seed(42)
index <- createDataPartition(bc_data$Class, p = 0.7, list = FALSE)
train_data <- bc_data[index, ] %>% select(-Id)
test_data  <- bc_data[-index, ] %>% select(-Id)

grid = expand.grid(C = c(0.8, 1, 1.2, 2), gamma = c(1, 5, 10))

# baseline by rf in Caret
model_rf <- train(Class ~ ., data = train_data, method = "rf", 
                  trControl = trainControl(method = "cv", number = 5))

final <- data.frame(actual = test_data$Class, predict(model_rf, test_data))
cm_original <- confusionMatrix(final$predict, test_data$Class)

# Under-sampling and Over-sampling by Caret
ctrl <- trainControl(method = "cv", number = 5, sampling = "down") # for over-sampling, you change down to up
# values supported in Caret sampling are "none", "down", "up", "smote", or "rose"

set.seed(42)
model_rf_under <- train(Class ~ ., data = train_data, method = "rf", 
                        trControl = ctrl)
final_under <- data.frame(actual = test_data$Class, predict(model_rf_under, test_data))
cm_under <- confusionMatrix(final_under$predict, test_data$Class)

# SMOTE by Caret
ctrl <- trainControl(method = "cv", number = 5, sampling = "smote")
set.seed(42)
model_rf_smote <- train(Class ~ ., data = train_data, method = "rf", trControl = ctrl)
final_smote <- data.frame(actual = test_data$Class, predict(model_rf_smote, test_data))

cm_smote <- confusionMatrix(final_smote$predict, test_data$Class)

# comparing performance
models <- list(original = model_rf,
               under = model_rf_under,
               smote = model_rf_smote
)

resampling <- resamples(models)
bwplot(resampling)

# Nicer plot for comparing results
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))

for (name in names(models)) {
  model <- get(paste0("cm_", name))
  metrics <- model$byClass
  
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(Sensitivity = metrics["Sensitivity"],
           Specificity = metrics["Specificity"],
           Precision = metrics["Precision"],
           Recall = metrics["Recall"],
           F1 = metrics["F1"])
}

comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)
