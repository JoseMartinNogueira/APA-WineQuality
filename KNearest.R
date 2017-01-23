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

#k-nearest

#whitewine
neighboursWW <- c(1:30)
errorsWW <- matrix (nrow=length(neighboursWW), ncol=2)
colnames(errorsWW) <- c("k","LOOCV error")

for (k in neighboursWW)
{
  myknn.cv <- knn.cv (whiteWine.learn.input, whiteWine.learn.classes, k = neighboursWW[k])
  errorsWW[k, "k"] <- neighboursWW[k]
  tabWW <- table(myknn.cv, whiteWine.learn.classes)
  errorsWW[k, "LOOCV error"] <- 1 - sum(tabWW[row(tabWW)==col(tabWW)])/sum(tabWW)
}

errorsWW

#redwine

neighboursRW <- c(1:30)
errorsRW <- matrix (nrow=length(neighboursRW), ncol=2)
colnames(errorsRW) <- c("k","LOOCV error")

for (k in neighboursRW)
{
  myknn.cv <- knn.cv (redWine.learn.input, redWine.learn.classes, k = neighboursRW[k])
  errorsRW[k, "k"] <- neighboursRW[k]
  tabRW <- table(myknn.cv, redWine.learn.classes)
  errorsRW[k, "LOOCV error"] <- 1 - sum(tabRW[row(tabRW)==col(tabRW)])/sum(tabRW)
}

errorsRW

#K = 1 es parece ser el mejor para ambos vinos

myknnWW <- knn (whiteWine.learn.input, whiteWine.test.input, whiteWine.learn.classes, k = 1, prob=TRUE) 

tabWW <- table(myknnWW, whiteWine.test.classes) 
tabWW
(errorWW <- 1 - sum(tabWW[row(tabWW)==col(tabWW)])/sum(tabWW))

myknnRW <- knn (redWine.learn.input, redWine.test.input, redWine.learn.classes, k = 1, prob=TRUE) 

tabRW <- table(myknnRW, redWine.test.classes) 
tabRW
(errorRW <- 1 - sum(tabRW[row(tabRW)==col(tabRW)])/sum(tabRW))