# easy exampes codes on mushroom dataset.
# install.packages("RWeka", dependencies=TRUE)
# install.packages("rJava", dependencies=TRUE)
library(RWeka)

# read input files
mushrooms <- read.csv("d://RData//mushrooms.csv", stringsAsFactors = TRUE)

# take a look at the data
str(mushrooms)
summary(mushrooms)
# By summary, you should be able to see one variable seems to have problem, this is a good example that you should
# always to through the dataset's descriptive statistics one in the beginning. You can just open this datafile by exel
# and you will observe what is wrong. The textbook use the following command to drop that variable.
mushrooms$veil_type <- NULL

# Take a look at the label's distribution again
table(mushrooms$type)

# The main OneR command: One R model
mushroom_1R <- OneR(type ~ . , data = mushrooms)

# To examine the rules it created, we can type the name of the classifier object
mushroom_1R

# summary function of this object gives you the confusion matrix
# the accuracy is quite high
summary(mushroom_1R)

# RIPPER
# For a more sophisticated rule learner, we will use JRip(), a Java-based implementation of the RIPPER rule learning algorithm. As with the 1R implementation
# we used previously, JRip() is included in the RWeka package. 
# RWeka package could be difficult to install if you are using an old version R.
mushroom_JRip <- JRip(type ~., data = mushrooms)

# check rules
mushroom_JRip

# you can check confusion matrix here
# In this extreme case, we can perfectly classify all cases by 9 simple rules (simple because only two conditions at most)
summary(mushroom_JRip) 
