rm(list=ls())
library(tm)
library(DBI)
library(RMySQL)
library(e1071)
library(pROC)
library(plyr)
library(data.table)
#library(caTools)

setwd("C:/Fall b/Social Media/project")

final_data = read.csv("ComplaintData.csv")

#############   Evaluation   ##############

Evaluation <- function(pred, true, class)
{
  tp <- sum( pred==class & true==class)
  fp <- sum( pred==class & true!=class)
  tn <- sum( pred!=class & true!=class)
  fn <- sum( pred!=class & true==class)
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  F1 <- 2/(1/precision + 1/recall)
  F1
}

set.seed(362)

#######  Supervised Classification  #######

Y = as.numeric(final_data$complaint)

docs <- Corpus(VectorSource(final_data$tweet))
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english")), stripWhitespace=T)
dtm_full <- DocumentTermMatrix(docs, control=dtm.control)
dtm <- removeSparseTerms(dtm_full,0.99)
X <- as.matrix(dtm)


#### Generate a training set that is 70% of the data, and a validation set that is 30% #####
n=length(Y)
n_1=round(n*0.70)
n_2=n-n_1
train=sample(1:n,n_1)

#######   Support Vector Machine   ########

svm_model <- svm(Y[train] ~ ., data = X[train,], kernel='linear')
pred <- predict( svm_model, X[-train,] )
pred.class <- as.numeric( pred>0.1 ) # try varying the threshold distance
table(pred.class, Y[-train])
Evaluation( pred.class, Y[-train], 1 )




#################### TEST ######################

test = read.csv("temp.csv")
docs_test <- Corpus(VectorSource(test$tweet))

dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english")), stripWhitespace=T)
dtm_test <- DocumentTermMatrix(docs_test, control=dtm.control)
dtm1 <- removeSparseTerms(dtm_test,0.99)
X_test <- as.matrix(dtm1)


############### DTM matching ############

#subset testing data by colnames (ie. terms) or training data
xx <- data.frame(X_test[,intersect(colnames(X_test),
                               colnames(X))])
# make an empty data frame with the colnames of the training data
yy <- read.table(textConnection(""), col.names = colnames(X),
                 colClasses = "integer")

xx = data.table(xx)
zz <- rbind.fill(xx, data.table(yy))
colnames(zz)[colnames(zz) == 'next.'] = 'next'
zz = data.table(zz)
zz = as.matrix(zz)
zz[is.na(zz) == T] = 0


#################### TEST SVM ######################
pred_svm <- predict( svm_model, zz )
test$pred.class <- as.numeric( pred_svm>0.2 ) # try varying the threshold distance

summary(as.factor(pred.class))

write.csv(test, 'final_tweets.csv', row.names = FALSE)

###########################################
##########   Naive Bayes   #############

nb.model <- naiveBayes( X[train,], factor( Y[train]) ) # encode the response as a factor variable
pred.class <- predict( nb.model, X[-train,] )
table( pred.class, Y[-train] )
Evaluation( pred.class, Y[-train], 1 )

pred <- predict( nb.model, X[-train,], type = "raw" )
nb.roc <- roc( Y[-train], pred[,2] )
plot.roc( nb.roc )
auc( Y[-train], pred[,2] )


################## TEST NB ########################
pred_test_nb <- predict( nb.model, X_test )
summary(as.factor(pred_test_nb))

svm.roc <- roc( Y[-train], pred )
plot.roc( svm.roc )
auc( Y[-train], pred )

