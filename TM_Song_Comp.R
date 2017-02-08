library(XML)
library(tm)
library(readr)

## Read in Data
roll <- read.csv("RollingStones-lyrics.csv")
beat <- read.csv("Beatles-lyrics.csv")

## Check for Duplicates
anyDuplicated(roll) # 28
anyDuplicated(beat) # 152

# Remove Duplicates
roll2 <- unique(roll)
beat2 <- unique(beat)

# Assign binary operators to Artist
# Created as a new column
roll2["Artist"] <- 0
beat2["Artist"] <- 1

# Combining the dataframe of Beatles and the Rolling Stones dataframe into one dataframe
song <- rbind(roll2, beat2)

# Only care about words
beat_rs_words <- as.data.frame(song[,"lyrics"], stringsAsFactors = FALSE)

# Convert to Corpus
beat_rs = VCorpus(DataframeSource(beat_rs_words))

# regular indexing returns a sub-corpus
inspect(beat_rs[1:2])

# double indexing accesses actual documents
beat_rs[[1]]
beat_rs[[1]]$content

# compute TF-IDF matrix for Beatles and Rolling Stones
beat_rs.tfidf = DocumentTermMatrix(beat_rs, control = list(weighting = weightTfIdf))
beat_rs.tfidf

# there's a lot in the documents that we don't care about. clean up the corpus.
beat_rs.clean = tm_map(beat_rs, stripWhitespace)                          # remove extra whitespace
beat_rs.clean = tm_map(beat_rs.clean, removeNumbers)                      # remove numbers
beat_rs.clean = tm_map(beat_rs.clean, removePunctuation)                  # remove punctuation
beat_rs.clean = tm_map(beat_rs.clean, content_transformer(tolower))       # ignore case
beat_rs.clean = tm_map(beat_rs.clean, removeWords, stopwords("english"))  # remove stop words
beat_rs.clean = tm_map(beat_rs.clean, stemDocument)                       # stem all words

beat_rs[[1]]$content
beat_rs.clean[[1]]$content

# recompute TF-IDF matrix
beat_rs.clean.tfidf = DocumentTermMatrix(beat_rs.clean, control = list(weighting = weightTfIdf))
beat_rs.clean.tfidf

# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(beat_rs.clean.tfidf, 0.92)  # remove terms that are absent from at least 99% of documents (keep most terms)
as.matrix(tfidf.99[1:5,1:5])
tf <- as.matrix(tfidf.99)
beat_rs_final <- as.data.frame(tf)
tfidf.99

# Creating a data frame with words and binary artist as columns
corpus_with_response <- data.frame(cbind(song[,"Artist"], beat_rs_final))

# Logistic model
logistic_model <- glm(corpus_with_response$song....Artist.. ~., data = corpus_with_response)
summary(logistic_model)

## Subset selection based off of the logistic model

lm.null <- glm(corpus_with_response$song....Artist..~1, data = corpus_with_response)
lm.full <- glm(corpus_with_response$song....Artist..~., data = corpus_with_response)

## Forward selection
step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="forward")
beat_rs_bestModel <- glm(formula = corpus_with_response$song....Artist.. ~ love + know + just + babi + put + call + face + leav + see + will + 
  good + stop + get + walk + around + run + yeah + say + time +  mind + aint + hear + hard + turn + heart + gonna + caus + 
  better + ever + night + give, data = corpus_with_response, family = "binomial")

#######################
## IMPORT NEW ARTIST ##
#######################

# Read in Data
who <- read.csv("Who-lyrics.csv")

## Check for Duplicates
anyDuplicated(who) # 31

# Remove Duplicates
who2 <- unique(who)

# Only care about words
who_words <- as.data.frame(who2[,'lyrics'], stringAsFactors = FALSE)

# Convert to Corpus
who_corp <- VCorpus(DataframeSource(who_words))

# regular indexing returns a sub-corpus
inspect(who_corp[1:2])

# double indexing accesses actual documents
who_corp[[1]]$content

# Create a DocumentTermMatrix for the corpus, containing the Term Frequencies.
# Then, turn that into a data frame for manipulation.
beat_rs.tf = DocumentTermMatrix(beat_rs.clean)
beat_rs.tf<-as.data.frame(as.matrix(beat_rs.tf))
beat_rs.tf2 <- beat_rs.tf

# Create empty vector, with NAs for each column (term) in the corpus
doc_freq <- rep(NA, ncol(beat_rs.tf))

# Set each term to 1 if the term appears in the document, then sum each column 
# to find the DOCUMENT FREQUENCY. Insert this into the corresponding location in
# the above empty vector.
for (i in 1:ncol(beat_rs.tf)){
  for (j in 1:nrow(beat_rs.tf)){
    if (beat_rs.tf[j,i] > 0){
      beat_rs.tf2[j,i] = 1
    }
    doc_freq[i] <- sum(beat_rs.tf2[,i])
  }
}

# Calculate IDF for the corpus
n <- nrow(beat_rs.tf)
idf <- log(n/doc_freq)
idf <- as.data.frame(as.matrix(idf)) 

who.clean = tm_map(who_corp, stripWhitespace)                          # remove extra whitespace
who.clean = tm_map(who.clean, removeNumbers)                      # remove numbers
who.clean = tm_map(who.clean, removePunctuation)                  # remove punctuation
who.clean = tm_map(who.clean, content_transformer(tolower))       # ignore case
who.clean = tm_map(who.clean, removeWords, stopwords("english"))  # remove stop words

who.clean.tf <- DocumentTermMatrix(who.clean)
who.clean.tf

tf_who.92 = removeSparseTerms(who.clean.tf, 0.92)  # remove terms that are absent from at least 99% of documents (keep most terms)
as.matrix(tf_who.92[1:5,1:5])
tf <- as.matrix(tf_who.92)
who_final <- as.data.frame(tf)
tf_who.92

# Calculate TF-IDF for new document
new_doc <- who_final # NEW DOCUMENT HERE
new_tf_idf <- data.frame(matrix(nrow = nrow(new_doc),ncol = ncol(new_doc)))
names(new_tf_idf) <- names(new_doc)
  for (i in 1:ncol(new_doc)){
    new_tf_idf[,i] <- new_doc[,i]*idf[i,]
  }

# Create TF matrix for new artist
who.tf <- DocumentTermMatrix(who_corp)

#### SCRATCH ####
new_tf_idf2 <- new_tf_idf
for (i in names(beat_rs_final)){
  if (!i %in% names(new_tf_idf2)){
    new_tf_idf2 <- cbind(new_tf_idf2, "temp" = rep(0))
    names(new_tf_idf2)[[ncol(new_tf_idf2)]] <- i
  }
}


library(ROCR)

ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')
summary(pred)

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))

