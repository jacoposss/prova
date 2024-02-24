####eSERCIZIO 3####
albero = tree(sistolica ~.-id, data = pressure)
plot(albero)
text(albero, pretty = 0)
####ESERCIZIO 4####
set.seed(123)
training = cardio[1:100,]
test = cardio[101:150,]
train.x = training[,3:6]
test.x = test[,3:6]
train.y = training[,2]
mod.knn = knn(train.x,test.x,train.y,k=5)
table(Predicted = mod.knn, Observed = test[,2])
err = 1-mean(mod.knn == test[,2])

mod.knn2 = knn(train.x,test.x,train.y,k=2)
err = 1-mean(mod.knn2 == test[,2])
#Non mi fido del mio codice quindi uso anche la 
#confusion matrix
table(Pred = mod.knn2, Obs = test[,2])
