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

whiteWine <- read.table(file = "winequality-white.csv", sep = ";", header = TRUE)
redWine <- read.table(file = "winequality-red.csv", sep = ";", header = TRUE)

summary(whiteWine)
summary(redWine)
table(whiteWine$quality)
table(redWine$quality)
dim(whiteWine)
dim(redWine)
obsWW <- nrow(whiteWine)
obsRW <- nrow(redWine)


#outlayers

quartilesWW <- c()
for(i in seq(1:11)) {
  quartilesWW <- c(quartilesWW,as.numeric(quantile(whiteWine[,i])[4]))
}

quartilesRW <- c()
for(i in seq(1:11)) {
  quartilesRW <- c(quartilesRW,as.numeric(quantile(redWine[,i])[4]))
}

iqrWW <- c()
for(i in seq(1:11)) {
  iqrWW <- c(iqrWW, IQR(whiteWine[,i]))
}

iqrRW <- c()
for(i in seq(1:11)) {
  iqrRW <- c(iqrRW, IQR(redWine[,i]))
}

for(i in seq(1:obsWW)) {
  for(j in seq(1:11)) {
    if (whiteWine[i,j] > quartilesWW[j] + 1.5*iqrWW[j]) {
      whiteWine[i,j] <- NA
    }
  }
}

for(i in seq(1:11)) {
  for(j in seq(1:11)) {
    if(redWine[i,j] > quartilesRW[j] + 1.5*iqrWW[j]) {
      redWine[i,j] <- NA
    }
  }
}

whiteWine <- na.omit(whiteWine)
redWine <- na.omit(redWine)

dim(whiteWine)
table(whiteWine$quality)

dim(redWine)
table(redWine$quality)



#fixed.acidity.
#wihitewine no mejora
#redWine mejora
hist(whiteWine$fixed.acidity)
hist(log(whiteWine$fixed.acidity))
skewness(whiteWine$fixed.acidity)
skewness(log(whiteWine$fixed.acidity))
kurtosis(whiteWine$fixed.acidity)

hist(redWine$fixed.acidity)
hist(log(redWine$fixed.acidity))
skewness(redWine$fixed.acidity)
skewness(log(redWine$fixed.acidity))
kurtosis(redWine$fixed.acidity)
kurtosis(log(redWine$fixed.acidity))
redWine$fixed.acidity <- log(redWine$fixed.acidity)

#volatile.acidity. 
#whitewine correcto
#redwine mejora

hist(whiteWine$volatile.acidity)
hist(log(whiteWine$volatile.acidity))
skewness(whiteWine$volatile.acidity)
skewness(log(whiteWine$volatile.acidity))
kurtosis(whiteWine$volatile.acidity)
kurtosis(log(whiteWine$volatile.acidity))

hist(redWine$volatile.acidity)
hist(log(redWine$volatile.acidity))
skewness(redWine$volatile.acidity)
skewness(log(redWine$volatile.acidity))
kurtosis(redWine$volatile.acidity)
kurtosis(log(redWine$volatile.acidity))
redWine$volatile.acidity <- log(redWine$volatile.acidity)

#citric.acid. 
#whitewine correcto
#redwine correcto
hist(whiteWine$citric.acid)
hist(log(whiteWine$citric.acid))
skewness(whiteWine$citric.acid)
skewness(log(whiteWine$citric.acid))
kurtosis(whiteWine$citric.acid)

hist(redWine$citric.acid)
hist(log(redWine$citric.acid))
skewness(redWine$citric.acid)
skewness(log(redWine$citric.acid))
kurtosis(redWine$citric.acid)

#residual.sugar. 
#whitewine mejora
#redwine mejora
hist(whiteWine$residual.sugar)
hist(log(whiteWine$residual.sugar))
skewness(whiteWine$residual.sugar)
skewness(log(whiteWine$residual.sugar))
kurtosis(whiteWine$residual.sugar)
kurtosis(log(whiteWine$residual.sugar))
whiteWine$residual.sugar <- log(whiteWine$residual.sugar) 

hist(redWine$residual.sugar)
hist(log(redWine$residual.sugar))
skewness(redWine$residual.sugar)
skewness(log(redWine$residual.sugar))
kurtosis(redWine$residual.sugar)
kurtosis(log(redWine$residual.sugar))
redWine$residual.sugar <- log(redWine$residual.sugar)

#chlorides. Parece correcto
hist(whiteWine$chlorides)
hist(log(whiteWine$chlorides))
skewness(whiteWine$chlorides)
skewness(log(whiteWine$chlorides))
kurtosis(whiteWine$chlorides)

hist(redWine$chlorides)
hist(log(redWine$chlorides))
skewness(redWine$chlorides)
skewness(log(redWine$chlorides))
kurtosis(redWine$chlorides)
kurtosis(log(redWine$chlorides))
redWine$chlorides <- log(redWine$chlorides)

#free.sulfur.dioxide.
#whitewine correcto
#redwine mejora
hist(whiteWine$free.sulfur.dioxide)
hist(log(whiteWine$free.sulfur.dioxide))
skewness(whiteWine$free.sulfur.dioxide)
skewness(log(whiteWine$free.sulfur.dioxide))
kurtosis(whiteWine$free.sulfur.dioxide)

hist(redWine$free.sulfur.dioxide)
hist(log(redWine$free.sulfur.dioxide))
skewness(redWine$free.sulfur.dioxide)
skewness(log(redWine$free.sulfur.dioxide))
kurtosis(redWine$free.sulfur.dioxide)
kurtosis(log(redWine$free.sulfur.dioxide))
redWine$free.sulfur.dioxide <- log(redWine$free.sulfur.dioxide)

#total.sulfur.dioxide. Parece correcto
#whitewine correcto
#redwine mejora
hist(whiteWine$total.sulfur.dioxide)
hist(log(whiteWine$total.sulfur.dioxide))
skewness(whiteWine$total.sulfur.dioxide)
skewness(log(whiteWine$total.sulfur.dioxide))
kurtosis(whiteWine$total.sulfur.dioxide)

hist(redWine$total.sulfur.dioxide)
hist(log(redWine$total.sulfur.dioxide))
skewness(redWine$total.sulfur.dioxide)
skewness(log(redWine$total.sulfur.dioxide))
kurtosis(redWine$total.sulfur.dioxide)
kurtosis(log(redWine$total.sulfur.dioxide))
redWine$total.sulfur.dioxide <- log(redWine$total.sulfur.dioxide)

#density. Parece correcto
#whitewine correcto
#redwine correcto
hist(whiteWine$density)
hist(log(whiteWine$density))
skewness(whiteWine$density)
skewness(log(whiteWine$density))
kurtosis(whiteWine$density)

hist(redWine$density)
hist(log(redWine$density))
skewness(redWine$density)
skewness(log(redWine$density))
kurtosis(redWine$density)
kurtosis(log(redWine$density))

#pH. Parece correcto
hist(whiteWine$pH)
hist(log(whiteWine$pH))
skewness(whiteWine$pH)
skewness(log(whiteWine$pH))
kurtosis(whiteWine$pH)

hist(redWine$pH)
hist(log(redWine$pH))
skewness(redWine$pH)
skewness(log(redWine$pH))
kurtosis(redWine$pH)
kurtosis(log(redWine$pH))

#sulphates. Mejora al aplicar el log
hist(whiteWine$sulphates)
hist(log(whiteWine$sulphates))
skewness(whiteWine$sulphates)
skewness(log(whiteWine$sulphates))
kurtosis(whiteWine$sulphates)
kurtosis(log(whiteWine$sulphates))
whiteWine$sulphates <- log(whiteWine$sulphates)

hist(redWine$sulphates)
hist(log(redWine$sulphates))
skewness(redWine$sulphates)
skewness(log(redWine$sulphates))
kurtosis(redWine$sulphates)
kurtosis(log(redWine$sulphates))
redWine$sulphates <- log(redWine$sulphates)

#alcohol
hist(whiteWine$alcohol)
hist(log(whiteWine$alcohol))
skewness(whiteWine$alcohol)
skewness(log(whiteWine$alcohol))
kurtosis(whiteWine$alcohol)
kurtosis(log(whiteWine$alcohol))
whiteWine$alcohol <- log(whiteWine$alcohol)

hist(redWine$alcohol)
hist(log(redWine$alcohol))
skewness(redWine$alcohol)
skewness(log(redWine$alcohol))
kurtosis(redWine$alcohol)
kurtosis(log(redWine$alcohol))


print("Correlacion variables y calidad")
namesWW <- names(whiteWine)
for(i in seq(1:11)) {
  correlacionWW <- cor(whiteWine[,i], whiteWine[,12])
  print(paste(namesWW[i], correlacionWW, sep=" : "))
}

namesRW <- names(redWine)
for(i in seq(1:11)) {
  correlacionRW <- cor(redWine[,i], redWine[,12])
  print(paste(namesRW[i], correlacionRW, sep=" : "))
}


#Parece que las clases ahora esta distribuidas de forma mas uniforme
table(whiteWine$quality)
table(redWine$quality)

#separar set de entrenamiento y set de test

whiteWine$quality <- as.factor(whiteWine$quality)
redWine$quality <- as.factor(redWine$quality)

testWW <- sample(1:nrow(whiteWine), nrow(whiteWine)/10)
testRW <- sample(1:nrow(redWine), nrow(redWine)/10)

write.table( testWW, file = "testWhiteWine.csv", row.names = FALSE, col.names = TRUE, sep = ";" )
write.table( testRW, file = "testRedWine.csv" )

write.table( whiteWine, file = "whiteWine-processed.csv", row.names = FALSE, col.names = TRUE, sep = ";" )
write.table( redWine, file = "redWine-processed.csv", row.names = FALSE, col.names = TRUE, sep = ";" )

