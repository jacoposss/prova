library(ISLR)
set.seed(123)

data(Auto)

## VALIDATION SET ##
indice = rep(TRUE, 392)

samp = sample(1:392, 196)

indice[samp] = FALSE

table(indice)

train <- Auto[indice,]
test <- Auto[!indice,]



## CROSS VALIDATION ##

library(boot)

mod.fit = lm(mpg ~ horsepower, data = train)
mod.fit

mod.pred = predict(mod.fit, test)
mod.pred

# creo una funzione per calcolare gli MSE
MSE <- function(osservazione, previsione){
  mean((osservazione-previsione)^2)
}

MSE(test$mpg, mod.pred)


###Polinomiali###

mod.fit2 = lm(mpg ~ poly(horsepower,2), data = train)
mod.pred2 = predict(mod.fit2, test)
MSE(test$mpg, mod.pred2)

mod.fit3 = lm(mpg ~ poly(horsepower,3), data = train)
mod.pred3 = predict(mod.fit3, test)
MSE(test$mpg, mod.pred3)

# se mi fermassi a tre gradi del polinomio scegliere il modello con due gradi
# perchè ha l'MSE più basso


# per calcolare il MSE di piu gradi uso un loop e stampo i risultati per ogni grado

for(i in 1:5){
  mod.fitloop = lm(mpg ~ poly(horsepower,i), data = train)
  mod.predloop = predict(mod.fitloop, test)
  print(MSE(test$mpg, mod.predloop))
}


####LOOCV####
# serve la funzione glm della libreria boot
# supponiamo di voler testare fino a 5 gradi del polinomio
# mi creo una variabile con 5 valori (in questo caso uso replace) (uno per ogni grado) che andra a contenere gli errori
# con un ciclo for per i gradi del polinomio (ma come data non prendo solo il training set ma tutto il set)
# con glm faccio il LOOCV
# parametri glm (setdidatiIntero, il modello, k) k indica il numero di gruppi (nel LOOCV ogni gruppo è di 1 elemento
# quindi equivale al numero di osservazioni nel campione)
# di tutti i parametri che restituisce glm ci interessa il parametro delta[1]
# ci andiamo a salvare tutti i delta[1] nell variabile degli errori con un ciclo

# delta[1] è la media degli errori quadratici nelle n osservazioni

cv.error = rep(0,5)
for(i in 1:5){
  mod.LOOCV = glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto, mod.LOOCV, K=392)$delta[1]
}

cv.error


####K-fold####
# è uguale al LOOCV ma con andando a cambiare k (fissiamo k = 7)
cv.errorK = rep(0,5)
for(i in 1:5){
  mod.KFOLD = glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.errorK[i] = cv.glm(Auto, mod.KFOLD, K=7)$delta[1]
}

cv.errorK




####METODO DA USARE PER VARIABILE Y CATEGORICA####
# serve il pacchetto caret
library(caret)
set.seed(123)

#la funzione trainControl ha due paaramteri:
# 1 metodo che voglio applicare al modello che gli passo dopo
# numero di volte (per LOOCV uguale al numero di osservazioni del dataset, k-fold gli imposti un valore)

train.control <- trainControl(method = 'cv', number = 10)
train.control


####Train del modello####

# il train del modello si fa con caret::train
# modello
# i dati
# il metodo
# trcontrol

model1 <- caret::train(as.factor(origin) ~ cylinders + horsepower + 
                         weight + acceleration,
                       data = Auto, method = 'lda', trControl = train.control)
model1

View(model1)

# tra gli elementi dell'output di caret::train ci interessa l'accuracy




####K-fold con KNN dati iris####
data(iris)

train.control1 = trainControl(method = 'cv', number = 5)

# tuneGrid:
# metric _

fit.knn <- caret::train(Species ~., data = iris, method = 'knn',
                        trControl = train.control1,
                        tuneGrid = expand.grid(k = 1:50),
                        metric = 'Accuracy')
fit.knn

# trovo l'accuracy massima
max(fit.knn$results[,2])



####LOOCV####
train.control2 <- trainControl(method = 'LOOCV')

fit.knn2 <- caret::train(Species ~ ., 
                         method = 'knn', tuneGrid = expand.grid(k = 1:20),
                         trControl = train.control2, data = iris,
                         metric = 'Accuracy')
fit.knn2


####LOOCV####
train.control3 <- trainControl(method = 'LOOCV')
fit.lm2 <- caret::train(mpg ~ horsepower, 
                        method = 'lm', 
                        trControl = train.control3, data = Auto)
fit.lm2




