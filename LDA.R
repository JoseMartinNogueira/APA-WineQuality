library(ggplot2)
library(gridExtra)
library(MASS)
library(class)
library(e1071) 
library(nnet)
library(caret)
library(randomForest)
library(cclust)

set.seed(1111)

whiteWine <- read.table(file = "whiteWine-processed.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "redWine-processed.csv", sep = ";", header = TRUE)

testWW <- read.csv(file = "testWhiteWine.csv", sep = ";", header = TRUE)
testRW <- read.table(file = "testRedWine.csv", sep = ";", header = TRUE)

whiteWine.learn <- whiteWine[-testWW,]
redWine.learn <- redWine[-testRW,]

whiteWine.test <- whiteWine[testWW,]
redWine.test <- redWine[testRW,]

whiteWine.learn.input <- whiteWine.learn[,1:11]
redWine.learn.input <- redWine.learn[,1:11]

whiteWine.learn.classes <- whiteWine.learn[,12]
redWine.learn.classes <- redWine.learn[,12]

whiteWine.test.input <- whiteWine.test[,1:11]
redWine.test.input <- redWine.test[,1:11]

whiteWine.test.classes <- whiteWine.test[,12]
redWine.test.classes <- redWine.test[,12]

#LDA

whiteWine.lda.cv <- lda(quality ~ ., data = whiteWine.learn, CV=TRUE)

tabWW <- table(whiteWine.learn$quality, whiteWine.lda.cv$class)  
(errorWW.LOOCV <- 100*(1-sum(tabWW[row(tabWW)==col(tabWW)])/sum(tabWW)))

modelWW.lda <- lda(quality ~ ., data = whiteWine.learn)

lda.predictionsWW <- predict(modelWW.lda, whiteWine.test)
lda.predictionsWW$class


tabWW <- table(whiteWine.test$quality, lda.predictionsWW$class)  
tabWW
errorWW <- 100*(1-sum(tabWW[row(tabWW)==col(tabWW)])/sum(tabWW))
errorWW


redWine.lda.cv <- lda(quality ~ ., data = redWine.learn, CV=TRUE)

tabRW <- table(redWine.learn$quality, redWine.lda.cv$class)  
(errorRW.LOOCV <- 100*(1-sum(tabRW[row(tabRW)==col(tabRW)])/sum(tabRW)))

modelRW.lda <- lda(quality ~ ., data = redWine.learn)

lda.predictionsRW <- predict(modelRW.lda, redWine.test)
lda.predictionsRW$class


tabRW <- table(redWine.test$quality, lda.predictionsRW$class)  
tabRW
errorRW <- 100*(1-sum(tabRW[row(tabRW)==col(tabRW)])/sum(tabRW))
errorRW