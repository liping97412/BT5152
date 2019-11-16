# It is often that later this semester, you will need to install pacakges.
# If you already installed a specific pacakge, then you can skip the following command
install.packages("class")

# load a library
# if you encounter an error that you do not have a function on your R, then it is likely that you forgot to load the library of that function.
# if you haven't install a libraray, then you need to use install.pacakges("XXX") to install it first
# if you do not know the name of the library or package, you can Google the function name to find out the documentation and package name
library(class)

# read input file
# csv is one of the most common data file for R. It is a text file that you can open by most text editors, and also Excel.
# CSV fileuses "," to separate two columns. 
# If you have textual column, it should be enclosed by "" to avoid the special character "," breaking the separation of columns.
# the package default is the first column is the header of your data
wbcd <- read.csv("d:/RData/wisc_bc_data.csv", stringsAsFactors = FALSE)
# when loading a file, there are several ways to state path of your file, this one is hard-coded and is the easiest to undertand.
# you need to change to where your file is.

# check the number of rows and number of features, you can skip this step if you are confident that the data is correct
str(wbcd)

# Let's drop the id feature altogether. As it is located in the first column, we can
# exclude it by making a copy of the wbcd data frame without column 1:
wbcd <- wbcd[-1]

# check the label feature distribution
table(wbcd$diagnosis)

# make sure this feature is a categorical variable, which is required by many packages
# Also, we change the label names to make it more informative (this step is optional)
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))

# check summary statistics
# as I mentioned in the first class, read through min and max once can help you identify a number of data processing mistakes
summary(wbcd)

# visualizing data by box plot
boxplot(wbcd$radius_mean, main="Boxplot of Radius Mean", ylab="radius_mean")

# visualizing data by histogram
hist(wbcd$radius_mean, main = "Histogram of Radius Mean", xlab = "radius_mean")

# visualizing data by scatter plots
plot(x = wbcd$radius_mean, y = wbcd$diagnosis, main = "Scatterplot of Y vs. X", xlab = "radius_mean", ylab = "diagnosis")

# if you feel there are too many columns, use the following to select few columns
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# define the normalize function
# the following is an important example for defining functions, if you know programming, this is straightfoward.
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x)))
}

# apply the normalize function to all numeric columns
# this is an important example about applying the function you created
wbcd_z <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_z)

# split the dataset into training set and test test
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# create vector/columns of labels for later use
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# the main step of KNN
# you can find the full manul on IVLE, this function only supports Euclidean distance KNN
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 10)

install.packages("descr")
library("descr")
# check the performance
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
