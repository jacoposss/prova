data(iris)

?iris

summary(iris)

hist(iris$Sepal.Length)

norm <- function(x){
  (x-min(x)) / (max(x)-min(x))
}

datasetNormalizzato <- as.data.frame(apply(iris[,-5], 2, norm))
datasetNormalizzato$Species = iris$Species


# creo training e testing set

indici <- rep(TRUE,150)
indiciTest <- sample(1:150, 50)

indici[indiciTest] = FALSE

trainingSet <- datasetNormalizzato[indici,]
testingSet <- datasetNormalizzato[!indici,]


# bartlet test
library(rstatix)

box_m(trainingSet[,1:4] , trainingSet[, 5])
# p-value sotto 0.05 ----> analisi discriminante quadratica

#qda
library(MASS)
modello <- qda(Species ~ ., data = trainingSet)

prediction <- predict(modello, testingSet)

table(previsioni = prediction$class, osservazioni = testingSet$Species)

accuratezza <- mean(prediction$class==testingSet$Species)
accuratezza

error <- 1-accuratezza
error
