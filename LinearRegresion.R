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

regLinealWW <- glm(as.numeric(quality)~., data = whiteWine.learn)
summary(regLinealWW)

predRegLinealWW.learn <- predict.glm( regLinealWW, whiteWine.test )
predErrorRLWW.learn <- sum(((as.numeric(whiteWine.learn$quality)-predRegLinealWW.learn)^2)/nrow(whiteWine.learn))
predErrorRLWW.learn

predRegLinealWW.test <- predict.glm( regLinealWW, whiteWine.test )
predErrorRLWW.test <- sum(((as.numeric(whiteWine.test$quality)-predRegLinealWW.test)^2)/nrow(whiteWine.test))
predErrorRLWW.test


regLinealRW <- glm(as.numeric(quality)~., data = redWine.learn)
summary(regLinealRW)

predRegLinealRW.learn <- predict.glm( regLinealRW, redWine.learn )
predErrorRLRW.learn <- sum(((as.numeric(redWine.learn$quality)-predRegLinealRW.learn)^2)/nrow(redWine.learn))
predErrorRLRW.learn

predRegLinealRW.test <- predict.glm( regLinealRW, redWine.test )
predErrorRLRW.test <- sum(((as.numeric(redWine.test$quality)-predRegLinealRW.test)^2)/nrow(redWine.test))
predErrorRLRW.test
