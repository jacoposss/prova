data(iris)
set.seed(1)

norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

datasetNormalizzato <- as.data.frame(apply(iris[,-5], 2, norm))

datasetNormalizzato$Species <- iris$Species

indici <- rep(TRUE, 150)
indiciTesting <- sample(1:150, 50)
indici[indiciTesting] <- FALSE

trainingSet <- datasetNormalizzato[indici,]
testingSet <- datasetNormalizzato[!indici,]


trainx <- trainingSet[,1:4]
trainy <- trainingSet$Species

testx <- testingSet[,1:4]
testy <- testingSet$Species

library(class)

mod <- knn(trainx,testx,trainy, k=10)

mod

table(predicted = mod, osservazion = testy)
accuratezza <- mean(mod==testy)
accuratezza


errori <- rep(NA, 20)

for(i in 1:20){
  mod <- knn(trainx,testx,trainy, k=i)
  errori[i] <- mean(mod == testy)
  cat('k = ' , i , " accuracy: " , errori[i] , '\n')
}
  
