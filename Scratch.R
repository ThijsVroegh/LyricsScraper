# Create a DocumentTermMatrix for the corpus, containing the Term Frequencies.
# Then, turn that into a data frame for manipulation.
news.clean.tf = DocumentTermMatrix(news.clean)
news.tf<-as.data.frame(as.matrix(news.clean.tf))
news.tf2 <- news.tf

# Create empty vector, with NAs for each column (term) in the corpus
doc_freq <- rep(NA, ncol(news.tf))


### Maybe this? sum(news.tf[,285] > 0)


# Set each term to 1 if the term appears in the document, then sum each column 
# to find the DOCUMENT FREQUENCY. Insert this into the corresponding location in
# the above empty vector.
for (i in 1:ncol(news.tf)){
  for (j in 1:nrow(news.tf)){
    if (news.tf[j,i] > 0){
      news.tf2[j,i] = 1
    }
  doc_freq[i] <- sum(news.tf2[,i])
  }
}

# Calculate IDF for the corpus
n <- nrow(news.tf)
idf <- log(n/doc_freq)

# Calculate TF-IDF for new document
new_tfi_df <- data.frame()
new_doc <- # NEED SOMETHING HERE!!
for (i in 1:nrow(new_doc)){
  new_tf_idf[i,] <- new_doc[i,]*idf
}

#### Cross Validation ####

library(leaps)

# remove terms that are absent from at least 90% of documents (keep most terms)
tfidf.90 <- removeSparseTerms(beat_rs.clean.tfidf, 0.96)  
tf <- as.data.frame(as.matrix(tfidf.90))
beat_rs_final <- as.data.frame(tf)


# Creating a data frame with words and binary artist as columns
corpus_with_response <- data.frame(cbind(song[,"Artist"], beat_rs_final))

# Create a vector assigning each item to a fold:
set.seed(4161984)
k=10
folds <- sample(1:k, nrow(corpus_with_response), replace = TRUE)

# Create vectors to store AIC, deviance and formulae for the 10 folds
cv.AIC<- rep(NA, 10)
cv.deviance <- rep(NA, 10)
cv.formula <- rep(NA, 10)

# create a loop for CV!

for(j in 1:k){
  lm.null <- glm(song....Artist..~1, data = corpus_with_response[folds != j, ], family = "binomial")
  lm.full <- glm(song....Artist..~., data = corpus_with_response[folds != j, ], family = "binomial")
  step.out <- step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
  cv.AIC[j] <- step.out$aic
  cv.deviance[j] <- step.out$deviance
  cv.formula[j] <- as.character(step.out$formula[3]) # just the variables!
}


# 90% Sparsity

which.min(cv.AIC) # Lowest AIC is model 10.
cv.formula[10]
# "love + just + know + call + babi + see + good + will + walk + gonna + caus +
# say + cant + right + time + mind + yes + yeah + eye + heart + tell + hear +
# around"

which.min(cv.deviance) # Lowest deviance is also model 10.


# 92% Sparsity

which.min(cv.AIC) # Lowest AIC is model 10.
cv.formula[10]
# "love + just + put + know + call + babi + face + leav + see + stop + gonna +
# good + walk + will + time + mind + yes + caus + yeah + eye + right + aint +
# get + youv"

which.min(cv.deviance) # Lowest deviance is model 4.
cv.formula[4]
# "love + put + just + know + babi + call + face + leav + stop + see + come +
# around + sun + will + walk + good + time + mind + turn + alway + run + yeah +
# get + better + caus + gonna + long + dont + still + night + yes + aint"


# 94% Sparsity

which.min(cv.AIC) # Lowest AIC is model XX.
cv.formula[7]
# love + babe + know + put + drink + till + just + call + babi + get + stop +
# ooh + light + street + ask + aint + good + yeah + high + shes + time + better
# + caus + new + blue + use + mine + around + see + that + give + walk + ill +
# mind + round + sweet + alway + leav + youll + night + turn + made + got +
# realli

which.min(cv.deviance) # Lowest deviance is model XX.
cv.formula[4]
# love + put + just + know + babi + babe + drink + call + high + stop + come +
# street + ask + aint + leav + good + light + alway + get + ooh + turn + blue +
# everyth + gonna + watch + yeah + around + sit + new + till + time + mind +
# walk + give + see + sweet + day + better + pleas + long + night + still + will
# + run + made + youv + anoth + seen


# 96% Sparsity

which.min(cv.AIC) # Lowest AIC is model XX.
cv.formula[7]
# cos + babe + know + love + put + till + drink + get + just + call + pain + ooh
# + though + chang + stop + light + youd + yeah + end + shes + high + feet +
# time + new + blue + use + better + close + sing + good + burn + mind + word +
# tonight + night + mine + line + walk + ask + aint + that + alway + town +
# sweet + made + bad + money + lose + man + your + pleas + mayb + today + took +
# morn + long + home + wanna + one + bed + world + alright + help + song +
# listen + hear + lost + heart + lone + year + \n    believ + leav + well + sun
# + right + sit + watch + gonna + need + smile + youv + sad + ill + youll + kiss
# + fall + without + treat + work + sometim + done + cri + play + woman + side +
# turn + see + tell + can + may + pass + hard + live + gotta + think

which.min(cv.deviance) # Lowest deviance is model XX.
cv.formula[7]


library(ROCR)

par(mfrow=c(3,3))

#### 90% ####

tf <- as.data.frame(as.matrix(beat_rs.clean.tfidf))

# Creating a data frame with words and binary artist as columns
corpus_with_response <- data.frame(cbind(song[,"Artist"], tf))

beat_rs_bestModel <- glm(formula = song....Artist.. ~ love + just + know + call + babi + see + good + will + walk + gonna + caus +
                           say + cant + right + time + mind + yes + yeah + eye + heart + tell + hear +
                           around, data = corpus_with_response, family = "binomial")
ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""), sub = "90% Sparsity")

#### 92% ####
beat_rs_bestModel <- glm(formula = song....Artist.. ~ love + just + put + know + call + babi + face + leav + see + stop + gonna +
                            good + walk + will + time + mind + yes + caus + yeah + eye + right + aint +
                            get + youv, data = corpus_with_response, family = "binomial")
ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""), sub = "92% Sparsity - AIC")

beat_rs_bestModel <- glm(formula = song....Artist.. ~ love + put + just + know + babi + call + face + leav + stop + see + come +
                            around + sun + will + walk + good + time + mind + turn + alway + run + yeah +
                            get + better + caus + gonna + long + dont + still + night + yes + aint, data = corpus_with_response, family = "binomial")
ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""), sub = "92% Sparsity - Deviance")

#### 94% ####
beat_rs_bestModel <- glm(formula = song....Artist.. ~ love + babe + know + put + drink + till + just + call + babi + get + stop +
                            ooh + light + street + ask + aint + good + yeah + high + shes + time + better
                            + caus + new + blue + use + mine + around + see + that + give + walk + ill +
                            mind + round + sweet + alway + leav + youll + night + turn + made + got +
                            realli, data = corpus_with_response, family = "binomial")
ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""), sub = "94% Sparsity - AIC")

beat_rs_bestModel <- glm(formula = song....Artist.. ~ love + put + just + know + babi + babe + drink + call + high + stop + come +
                            street + ask + aint + leav + good + light + alway + get + ooh + turn + blue +
                            everyth + gonna + watch + yeah + around + sit + new + till + time + mind +
                            walk + give + see + sweet + day + better + pleas + long + night + still + will
                            + run + made + youv + anoth + seen, data = corpus_with_response, family = "binomial")
ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""), sub = "94% Sparsity - Deviance")

#### 96% ####
beat_rs_bestModel <- glm(formula = song....Artist.. ~ cos + babe + know + love + put + till + drink + get + just + call + pain + ooh
                          + though + chang + stop + light + youd + yeah + end + shes + high + feet +
                          time + new + blue + use + better + close + sing + good + burn + mind + word +
                          tonight + night + mine + line + walk + ask + aint + that + alway + town +
                          sweet + made + bad + money + lose + man + your + pleas + mayb + today + took +
                          morn + long + home + wanna + one + bed + world + alright + help + song +
                          listen + hear + lost + heart + lone + year + believ + leav + well + sun
                          + right + sit + watch + gonna + need + smile + youv + sad + ill + youll + kiss
                          + fall + without + treat + work + sometim + done + cri + play + woman + side +
                          turn + see + tell + can + may + pass + hard + live + gotta + think, data = corpus_with_response, family = "binomial")
ground.truth.labels <- song["Artist"]
pred <- predict(beat_rs_bestModel, newdata = beat_rs_final, type='response')

# create ROCR prediction object and extract ROC curve and AUC
prediction = prediction(pred, ground.truth.labels)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""), sub = "96% Sparsity")
