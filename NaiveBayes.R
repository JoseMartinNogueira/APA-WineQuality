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

wwModelBayes <- naiveBayes( quality ~. , data = whiteWine.learn )
rwModelBayes <- naiveBayes( quality ~. , data = redWine.learn )

wwPred <- predict( wwModelBayes, whiteWine.learn )
rwPred <- predict( rwModelBayes, redWine.learn )

wwTable <- table( wwPred, whiteWine.learn$quality)
rwTable <- table( rwPred, redWine.learn$quality)

wwTable
wwerror <- 100*(1-sum(wwTable[row(wwTable)==col(wwTable)])/sum(wwTable))
wwerror
rwTable
rwerror <- 100*(1-sum(rwTable[row(rwTable)==col(rwTable)])/sum(rwTable))
rwerror

wwPred.test <- predict( wwModelBayes, whiteWine.test )
rwPred.test <- predict( rwModelBayes, redWine.test )

wwTable.test <- table( wwPred.test, whiteWine.test$quality)
rwTable.test <- table( rwPred.test, redWine.test$quality)

wwTable.test
wwerror.test <- 100*(1-sum(wwTable.test[row(wwTable.test)==col(wwTable.test)])/sum(wwTable.test))
wwerror.test
rwTable.test
rwerror.test <- 100*(1-sum(rwTable.test[row(rwTable.test)==col(rwTable.test)])/sum(rwTable.test))
rwerror.test