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

#Random Forest Regression

wwModelRF <- rfcv(whiteWine.learn.input, as.numeric(whiteWine.learn.classes), cv.fold =10)
wwModelRF$error.cv

(wwNtrees <- round(10^seq(1,3,by=0.2)))

wwRFResults <- matrix(rep(0,2*length(wwNtrees)),nrow=length(wwNtrees))
colnames(wwRFResults) <- c("ntrees", "OOB")
wwRFResults[,"ntrees"] <- wwNtrees
wwRFResults[,"OOB"] <- 0

ii <- 1

for(nt in wwNtrees) {
  print(nt)
  wwModelRF <- randomForest((quality) ~., data = whiteWine.learn, ntree=nt, proximity=FALSE)
  wwRFResults[ii,"OOB"] <- wwModelRF$err.rate[nt,1]
  ii <- ii+1
}
wwModelRF
wwRFResults

wwlowesOOB.error <- as.integer(which.min(wwRFResults[,"OOB"]))
(wwBestNtrees <- wwRFResults[wwlowesOOB.error,"ntrees"])