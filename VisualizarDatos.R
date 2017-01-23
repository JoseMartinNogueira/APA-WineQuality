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

#CLustering whitewine
dataWW = matrix(ncol=length(whiteWine.learn[1,]),nrow=length(whiteWine.learn[,1])) 
for (i in seq(1,length(whiteWine.learn[1,]), by=1)) {
  dataWW[,i] = whiteWine.learn[,i] 
}

K <- 10
kmeans <- cclust (dataWW, K, iter.max=100, method="kmeans", dist="euclidean")
(CH <- clustIndex(kmeans,dataWW, index="calinski"))

WWdata <- cbind(kmeans$cluster,dataWW)
WWdata <- as.data.frame(WWdata)
names(WWdata)[1]<-paste("Target")
WWdata$Target <- factor(WWdata$Target)

#LDA whiteWine
(lda.modelWW <- lda (Target ~ ., WWdata, prior = seq(1,1,length.out=K)/K))
plot(lda.modelWW, col = as.numeric((WWdata$Target)))

#Clustering redWIne

dataRW = matrix(ncol=length(redWine.learn[1,]),nrow=length(redWine.learn[,1])) 
for (i in seq(1,length(redWine.learn[1,]), by=1)) {
  dataRW[,i] = redWine.learn[,i] 
}

K <- 10
kmeans <- cclust (dataRW, K, iter.max=100, method="kmeans", dist="euclidean")
(CH <- clustIndex(kmeans,dataRW, index="calinski"))

RWdata <- cbind(kmeans$cluster,dataRW)
RWdata <- as.data.frame(RWdata)
names(RWdata)[1]<-paste("Target")
RWdata$Target <- factor(RWdata$Target)

#LDA redWine
(lda.modelRW <- lda (Target ~ ., RWdata, prior = seq(1,1,length.out=K)/K))
plot(lda.modelRW, col = as.numeric((RWdata$Target)))
