library(base)
library(tm)
library(dplyr)
library(reshape2)
library(ggplot2)
## read csv
transcripts <- read.csv("transcripts.csv")
ted_main <- read.csv("ted_main.csv")
## merge csv 
ted_data <- merge(ted_main,transcripts,by.x = 'url',by.y = 'url')
## sort data into two columns
ted_data_sorted <- data.frame(doc_id = as.vector(ted_data$title),text = as.vector(ted_data$transcript))
## construst corpus
ted_corpus <- Corpus(DataframeSource(ted_data_sorted))
ted_dtm <- DocumentTermMatrix(ted_corpus)
## find term freq in each document
features_ted <- as.data.frame(tbl_df(as.matrix(ted_dtm)))
## import packages
install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
require("tm.lexicon.GeneralInquirer")
## construct emot_words
emot_words <- terms_in_General_Inquirer_categories("EMOT")
emot_scores <- tm_term_score(ted_dtm, emot_words)
## construct emot_words freq in each document
results <- as.data.frame(matrix(NA,nrow=nrow(features_ted),ncol=length(emot_words)))
names(results) <- emot_words
for (feature in names(features_ted)){
  if (feature %in% emot_words){
    results[,feature] <- features_ted[,feature]
  }
}
results[is.na(results)]<-0
melted_results <- melt(results)
melted_results <- as.data.frame(cbind(rep(1:nrow(features_ted),length(emot_words)),melted_results))
names(melted_results) <- c('index','terms','freq')
##plot heat map
ggplot(data = melted_results, aes(x=terms, y=index, fill=freq)) + geom_tile()
