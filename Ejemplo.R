library(NLP)
library(tm)
texto <- c("Todos somos responsables de nuestros hechos. 17?", 
           "Los recursos humanos suelen ser formados por un hombre",
           "Conozco al hombre y amo a sus perro")
sms_corpus <- VCorpus(VectorSource(texto))

inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])

lapply(sms_corpus[1:2], as.character)

inspect(sms_corpus)

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords("spanish"))

as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]])

library(SnowballC)

wordStem(c("aprender", "aprendiendo", "aprendió"), "spanish")

#sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument, "spanish")

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]])

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm


sms_dtm2 <- DocumentTermMatrix(sms_corpus, 
                               control = list(
                                 tolower = TRUE,
                                 removeNumbers = TRUE,
                                 stopwords = TRUE,
                                 removePunctuation = TRUE,
                                 stemming = TRUE
                               ) 
)
findFreqTerms(sms_dtm2)

inspect(sms_dtm2)

datos<-data.frame(inspect(sms_dtm2))
datos

library(RColorBrewer)
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 1, random.order = FALSE)
