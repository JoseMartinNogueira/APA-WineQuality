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


#white wine

for(i in seq(1:11)) {
  whiteWine.learn[,i] <- scale(whiteWine.learn[,i])
  whiteWine.test[,i] <- scale(whiteWine.test[,i])
}

trc <- trainControl(method="repeatedcv", number=10, repeats=10)
decays <- 10^seq(-3,0,by=1)

wwModel.nnet <- train (quality ~., data = whiteWine.learn, method='nnet', maxit = 200, trace = FALSE,
                       tuneGrid = expand.grid(.size=10,.decay=decays), trControl=trc)
wwModel.nnet

wwModel.nnet <- nnet(quality~.,data = whiteWine.learn, size=50, maxit=500, decay=0.01)

wwPred.learn <- as.factor(predict(wwModel.nnet,type="class"))

wwTable.learn <- table(pred=wwPred.learn,whiteWine.learn$quality)
wwError_rate.learn <- 100*(1-sum(diag(wwTable.learn))/nrow(whiteWine.learn))
wwError_rate.learn

wwPred.test <- as.factor(predict (wwModel.nnet, newdata=whiteWine.test, type="class"))

wwTable.test <- table(pred=wwPred.test,truth=whiteWine.test$quality)
wwError_rate.test <- 100*(1-sum(diag(wwTable.test))/nrow(whiteWine.test))
wwError_rate.test

#red wine

for(i in seq(1:11)) {
  redWine.learn[,i] <- scale(redWine.learn[,i])
  redWine.test[,i] <- scale(redWine.test[,i])
}

rwModel.nnet <- train(quality ~., data = redWine.learn, method='nnet', maxit = 200, trace = FALSE,
                      tuneGrid = expand.grid(.size=10,.decay=decays), trControl=trc)
rwModel.nnet

rwModel.nnet <- nnet(quality~., data = redWine.learn, size=50, maxit=500, decay=0.01)

rwPred.learn <- as.factor(predict(rwModel.nnet,type="class"))

rwTable.learn <- table(pred=rwPred.learn, redWine.learn$quality)
rwError_rate.learn <- 100*(1-sum(diag(rwTable.learn))/nrow(redWine.learn))
rwError_rate.learn

rwPred.test <- as.factor(predict (rwModel.nnet, newdata=redWine.test, type="class"))

rwTable.test <- table(pred=rwPred.test,truth=redWine.test$quality)
rwError_rate.test <- 100*(1-sum(diag(rwTable.test))/nrow(whiteWine.test))
rwError_rate.test
