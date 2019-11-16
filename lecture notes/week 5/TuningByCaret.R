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

#Remove rows with missing values
BreastCancer <- na.exclude(BreastCancer)

########################
### Automated tuning ###
########################

# You can check tunable parameters with this function
# This is very important if you are using a new package with Caret
modelLookup("C5.0")

# Add cross-validation, and Caret also automatically try to tune your parameters
ctrl <- trainControl(method = "cv", number = 10)
# for repeated CV,
# ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# There are several other CV methods supported by Caret

# In the default case, Caret will choose 3 values per feature to tune
m <- train(Class ~ ., data = BreastCancer, method = "C5.0", trControl = ctrl)
# you can see the tuning results by checking the output object, it lists the grid search output of each case
m

# Caret is much more powerful than this simple command, you should develop the habit to check the manual.
# more features added into Caret every year.
help(trainControl)

# Grid Search
# Add your own customized grid search for performance tuning, you need to add all parameters into this function
grid <- expand.grid(.trials = c(5,10,15,20,25), .model = c("tree","rules"), .winnow = c(TRUE,FALSE))
# winnow is a feature selection, I typically dont use it but there is no harm trying
m <- train(Class ~ ., data = BreastCancer, method = "C5.0", trControl = ctrl, metric = "Kappa", tuneGrid = grid)
m

# again, learn more options of train function of Caret by reading its manual
help(train)

# algorithms supported by Caret,
# https://topepo.github.io/caret/available-models.html
# in other words, just by changing method = "C5.0" => method = "XXX", you can use many other algorithms for prediction or for classification

# You can also plot the performances with
plot(m)
