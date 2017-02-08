library(XML)
library(tm)

##### Constructing TF-IDF Matrices #####

setwd("~/Box Sync/Fall 2016/SYS 6018/Case Study 2")

library(readr)
roll <- read.csv("RollingStones-lyrics.csv")
beat <- read.csv("Beatles-lyrics.csv")
who <- read.csv("Who-lyrics.csv")
anyDuplicated(roll) # 28
anyDuplicated(beat) # 152
anyDuplicated(who) # 31

roll2 <- unique(roll)
beat2 <- unique(beat)
who2 <- unique(who)
#anyDuplicated(roll2)

roll2["Artist"] <- 0
beat2["Artist"] <- 1
song <- rbind(roll2, beat2)

# for the purpose of this example, we only care about content.
song_words <- as.data.frame(song[,"lyrics"], stringsAsFactors = FALSE)
who_words <- as.data.frame(who2[,'lyrics'], stringAsFactors = FALSE)
#song <- as.data.frame(roll2)
song_sub <- subset(song, select=c("lyrics"))

# convert this part of the data frame to a corpus object.
news = VCorpus(DataframeSource(song_words))
who_corp <- VCorpus(DataframeSource(who_words))
# regular indexing returns a sub-corpus
inspect(news[1:2])

inspect(who_corp[1:2])

# double indexing accesses actual documents
news[[1]]
news[[1]]$content

who_corp[[1]]$content

# compute TF-IDF matrix and inspect sparsity
news.tfidf = DocumentTermMatrix(news, control = list(weighting = weightTfIdf))
news.tfidf

who.tfidf <- DocumentTermMatrix(who_corp, control = list(weighting = weightTfIdf))
who.tfidf

# inspect sub-matrix:  first 5 documents and first 5 terms
as.matrix(news.tfidf[1:5,1:5])
news.tfidf[1:5,1:5]

##### Reducing Term Sparsity #####

# there's a lot in the documents that we don't care about. clean up the corpus.
news.clean = tm_map(news, stripWhitespace)                          # remove extra whitespace
news.clean = tm_map(news.clean, removeNumbers)                      # remove numbers
news.clean = tm_map(news.clean, removePunctuation)                  # remove punctuation
news.clean = tm_map(news.clean, content_transformer(tolower))       # ignore case
news.clean = tm_map(news.clean, removeWords, stopwords("english"))  # remove stop words
#news.clean = tm_map(news.clean, stemDocument)                       # stem all words

who.clean = tm_map(who_corp, stripWhitespace)                          # remove extra whitespace
who.clean = tm_map(who_corp, removeNumbers)                      # remove numbers
who.clean = tm_map(who_corp, removePunctuation)                  # remove punctuation
who.clean = tm_map(who_corp, content_transformer(tolower))       # ignore case
who.clean = tm_map(who_corp, removeWords, stopwords("english"))  # remove stop words


# compare original content of document 1 with cleaned content
news[[1]]$content
news.clean[[1]]$content

who.clean[[1]]$content

# recompute TF-IDF matrix
news.clean.tfidf = DocumentTermMatrix(news.clean, control = list(weighting = weightTfIdf))
news.clean.tfidf

who.clean.tfidf = DocumentTermMatrix(news.clean, control = list(weighting = weightTfIdf))
who.clean.tfidf

# reinspect the first 5 documents and first 5 terms
as.matrix(news.clean.tfidf[1:5,1:5])
news.clean.tfidf[1:5,1:5]

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(news.clean.tfidf, 0.985)  # remove terms that are absent from at least 99% of documents (keep most terms)
as.matrix(tfidf.99[1:5,1:5])
tf <- as.matrix(tfidf.99)
final <- as.data.frame(tf)
tfidf.99


w.tfidf.99 = removeSparseTerms(who.clean.tfidf, 0.985)  # remove terms that are absent from at least 99% of documents (keep most terms)
as.matrix(w.tfidf.99[1:5,1:5])
w.tfidf.99
who_matrix <- as.matrix(w.tfidf.99)
who_lyrics <- as.data.frame(who_matrix)


#tfidf.70 = removeSparseTerms(news.clean.tfidf, 0.70)  # remove terms that are absent from at least 70% of documents
#as.matrix(tfidf.70[1:5, 1:5])
#tfidf.70

## Put it all together to be able to run model
fin <- data.frame(cbind(song[,"Artist"], final))

# Logistic model
log <- glm(fin$song....Artist.. ~., data = fin)
summary(log)

# Try to predict where the Who fall, either higher probability of Beatles or Stones

pred_who <- predict(log, newdata = who_lyrics, type="response")


## Analysis of the logistic model
residuals(log)

sum(residuals(log, type = "pearson")^2)

deviance(log)

1 - pchisq(deviance(log), df.residual(log))

plot(log)

qqnorm(resid(log))
qqline(resid(log))

plot(log$fitted.values, residuals(log))

library("DAAG")

cv.lm(data=fin, form.lm=log, m=10, plotit=F)

# Started looking into some LDA stuff
library(MASS)
lda <- lda(fin$song....Artist.. ~., data = fin)
lda

plot(lda)


# Evaluation Techniques with code
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
# http://www.rdatamining.com/examples/text-mining
