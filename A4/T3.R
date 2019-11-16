library(base)
library(tm)
library(dplyr)
library(jsonlite)
library(caret)

transcripts <- read.csv("transcripts.csv")
ted_main <- read.csv("ted_main.csv")
ted_data <- merge(ted_main,transcripts,by.x = 'url',by.y = 'url')
ted_data$ratings <- gsub("'",'"', ted_data$ratings)
s_ratings <- vector()
for (i in 1:nrow(ted_data)){
  c_ratings <- fromJSON(ted_data$ratings[i])
  s_ratings[i] <- c_ratings[which(c_ratings$count==max(c_ratings$count))[1],2]
  }
ted_corpus <- Corpus(VectorSource(ted_data$transcript))
meta(ted_corpus, "rating", type = "indexed") <- s_ratings
train_ratings <- as.factor(meta(ted_corpus)$rating)
dtm_control <- list(weighting = weightTf, tolower = TRUE, removeNumbers = TRUE, stopwords = TRUE, removePunctuation = TRUE)
ted_dtm<-DocumentTermMatrix(ted_corpus,control=dtm_control)
freq_terms <- findFreqTerms(ted_dtm, lowfreq = 10)
dtm_control$dictionary<-freq_terms
ted_dtm<-DocumentTermMatrix(ted_corpus,control=dtm_control)
features_dtm <- tbl_df(as.matrix(ted_dtm)) 
nzv_columns <- nearZeroVar(features_dtm)
features_dtm <- features_dtm[, -nzv_columns]
model_control <- trainControl(method = "cv", number = 5)
model <- train(features_dtm, train_ratings, method = "nb", trControl = model_control)
pred <- predict(model$finalModel, features_dtm)
confusionMatrix(pred$class, train_ratings)

