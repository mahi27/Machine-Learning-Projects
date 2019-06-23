raw_data <- read.table("sms_spam.csv", sep = ",", header = TRUE)
table(raw_data[,1])
str(raw_data)
library(tm)
sms_corpus <- VCorpus(VectorSource(raw_data$text))
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
clean_corpus <- tm_map(sms_corpus, content_transformer(tolower))
clean_corpus <- tm_map(sms_corpus, removeNumbers)
clean_corpus <- tm_map(sms_corpus, removeWords, stopwords())
clean_corpus <- tm_map(sms_corpus, removePunctuation)

library(SnowballC)
clean_corpus <- tm_map(clean_corpus, stemDocument)
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

sms_dtm <- DocumentTermMatrix(clean_corpus)
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_train_labels <- raw_data[1:4169, ]$type
sms_test_labels <- raw_data[4170:5559, ]$type

table(sms_train_labels)
table(sms_test_labels)

library(wordcloud)
wordcloud(clean_corpus, min.freq = 50, random.order = FALSE)

spam <- subset(raw_data, type == "spam")
ham <- subset(raw_data, type == "ham")

wordcloud(spam$text, max.words = 50, scale = c(3,0.5))
wordcloud(ham$text, max.words = 50, scale = c(3,0.5))


freq_words <- findFreqTerms(sms_dtm_train, 5)
str(freq_words)

freq_train<- sms_dtm_train[ ,freq_words]
freq_test <- sms_dtm_test[ ,freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(freq_test, MARGIN = 2,
                    convert_counts)
library(e1071)
model <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
pred <- predict(model, sms_test, type = "class")
library(gmodels)
CrossTable(pred, sms_test_labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))


