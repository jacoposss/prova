## ESERCIZIO 3 ESAME 16 GENNAIO 2024

norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

datasetNormalizzato <- as.data.frame(apply(vino[,-1] , 2, norm))
datasetNormalizzato$Tipo <- vino$Tipo


library(MASS)

lda(datasetNormalizzato$Tipo ~ ., datasetNormalizzato)

trainingSet <- datasetNormalizzato[1:100,]
testingSet <- datasetNormalizzato[101:178,]


modello <- lda(trainingSet$Tipo ~ ., trainingSet)
modello

prediction <- predict(modello, testingSet)

table(previsioni = prediction$class, osservazioni = testingSet$Tipo)

accuratezza <- mean(prediction$class == testingSet$Tipo)
error <- 1-accuratezza
error
