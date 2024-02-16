# ESERCIZIO 4 ESAME 30 GIUNGO 23

# elimino l'ID del paziente che non serve per la classificazione
cardio <- cardio[,-1]

norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

trainingSet <- cardio[1:100, ]
testingSet <- cardio[101:150, ]

trainx <- trainingSet[,2:4]
trainy <- trainingSet$cardio

testx <- testingSet[,2:4]
testy <- testingSet$cardio


library(class)

mod <- knn(trainx, testx, trainy, k=5)

table(predicted = mod, osservazioni = testy)

errormod <- 1-mean(mod==testy)
errormod


mod2 <- knn(trainx, testx, trainy, k=2)

errormod2 <- 1-mean(mod2==testy)
errormod2
