########## CLASSIFICAZIONE #########

###### funzione di standardizzazione 

# iris è un dataset che contiene 4 caratteristiche riguardanti i fiori +  la y che è la specie
# si vogliono usare le 4 categorie per classificare i fiori per specie

data(iris)

?iris

head(iris)

table(iris$Species) # il table su una variabile categorica restitusice le numerosità di ogni categoria

iris

## analisi esplorativa nel dataset ##

dim(iris) # 150 unità e 5 colonne (attributi)

summary(iris)


# verifico l'assunzione di normalità
cor(iris[,-5])

# per fare questo utilizzo hist (fa visualizzare l'instogramma)
hist(iris$Petal.Width)
hist(iris$Petal.Length)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)

# non tutti gli atttributi sono distribuiti normalmente (solo sepal.width è distribuito come una normale)
# per questo motivo l'applicazione dell'analisi discriminante lineare con tutti gli attributi è sbagliato




# normalizzo e standardizzo i dati
# in R non esiste una funzione per normalizzare, quindi la creo
nor <- function(x){
  (x - min(x)) / (max(x) - (min(x)))
}

# creo il dataset di dati normalizzati (mettere sempre as.data.frame oppure apply trasforma in matrice)
# non serve normalizzare la 5 perchè è la categorica quindi la escludo dal risultato

iris.norm = as.data.frame(apply(iris[,-5], 2, nor))

# in questo modo il database normalizzato ha solo le 4 variabili normalizzate
# aggiungo la varaibile categorica prendendolo dall'ultima colonna del dataset originale

iris.norm$Species <- iris$Species




####### VALIDATION SET ##########

# divido il campione i due parti:
# training sample
# testing sample

set.seed(2803) # il seed serve ad impostare dati replicabili (basta che si richiama lo stesso seed)

# per creare i dataset devo creare un indice di 150 valori e distinguere i valori in:
# 100 TRUE --> indicano i 100 elementi del training set    50 --> indicano i 50 elementi del testing set

# per fare questo creo un indice con tutti valori TRUE per poi sostituirne 50 con FALSE

# rep prende 2 parametri (il valore che vuoi dare a tutti gli elementi, il numero di elementi)
ind = rep(TRUE, 150)

# a questo punto uso sample per campionare dei valori tra un range di valori
# campioniamo 50 valori tra 1 e 150 e nel set degli indici fisso gli indici campionati casualmente a FALSE

samp = sample(1:150, 50)
ind[samp] = FALSE


# usando il set di indici creo i set di training e testing set a partire del dataset orginario
# selezionando in train solo gli indici TRUE e in testo solo gli indici FALSE (!true)
train = iris.norm[ind,]
test = iris.norm[!ind,]


table(train$Species)
table(test$Species)




## ANALISI DISCRIMINANTE LINEARE ##
# per fare l'analisi disciminante lineare dobbiamo utilizzare una funzione all'interno della libreria "MASS"
library(MASS) 

# la funzione lda nella classe MASS permette di calcolare l'analisi disciminante lineare

# lda prende come parametri (modello, dati)


lda.fit <- lda(Species ~. , train)

lda.fit

# se stampiamo il modello visualizziamo

# 1 le prior probabilities dei gruppi:
# notiamo che nel training le prior sono: 0.33 Setosa -- 0,39 Versicolor -- 0,28 Virginica

# 2 le medie delle caratteristiche all'interno di ognuna delle classi
# per esempio all'interno della classe setosa sepal.lenght ha una media di 0,198
##########  all'interno della classe versicolor sepal.width ha una media di 0,314
##########  all'interno della classe virginica petal.lenght ha una media di 0,77

# 3 i coefficienti delle discriminanti lineari: le disciminanti lineari sono le rette che tagliano il dataset
      # ho tre classi quindi due discriminanti (perchè taglio lo spazio in 3)

# 4 proportion of trace: indica la percentuale di osservazioni che ogni tagli separa



# per fare previsione usando il testing set uso predict (modello addestrato, testing set)
lda.pred <- predict(lda.fit, test)

# predict restituisce tre elementi:
# class: è la classe nella quale l'algoritmo ha classificato le unità del testing set

# posterior

# x

lda.pred$class

lda.class <- lda.pred$class # creo un vettore contenente le previsioni




# per vedere se le previsioni fatte dal modello sono giuste utilizzo la confusion matrix
# per fare la confusion matrix si usa il comando multitasking table  --> restitusice una matrice di frequenze assolute
# o sul singolo valore o su piu argomenti

# parametri       i valori predetti e le classi delle unità di test

table(pred=lda.class, obs=test$Species)
# vediamo che c'è solo una missclassificazione dove il modello ha predetto versicolor al posto di virginica


#trovo accuratezza e errore
# l'accuratezza si trova facendo la media ponendo come condizione (classiPredette == classiDelTest)
# in questo modo fai la proprozione sul totale del numero di volto che classe predette ed osservata sono uguali
acc<- mean(lda.class==test$Species)
acc #98%

# l'errore si trova come 1- accuratezza'
err<- 1-acc
err #2%



##### test di barlett per vedere se la varianza è costante tra le classi 
# se non è costante -------> analisi discriminante quadratica

# una caratteristica
bartlett.test(Petal.Width ~ Species, train) 
#è significativo 

# piu caratteristiche
library(rstatix)
box_m(train[, 1:4], train[, 5])

# sulla base del test di bartlett per questo set si doveva usare l'analisi discriminante quadrativa



###### QDA #######
qda.fit <- qda(Species ~. , train)
qda.fit

qda.pred <- predict(qda.fit, test)

table(pred=qda.pred$class, obs=test$Species)
acc<- mean(qda.pred$class==test$Species)
acc
err<- 1-acc
err




######## k-nn ########
library(class)
set.seed(1)

# mi creo 4 oggetti
# varaibile contenente le x del training set
# varaibile contenente le x del testing set

# varaibile contenente le y del training set
# varaibile contenente le y del testing set

trainx <- train[,1:4] 
testx <- test[,1:4]

trainy <- train$Species
testy <- test$Species

knn.class <- knn(trainx, testx, trainy, k=4)
knn.class

# confusion matrix
table(pred=knn.class, obs=testy)

#errore e accuracy
accuracy = mean(knn.class == testy)
accuracy
err <- 1-accuracy
err



####Scelta di K####
k.err = rep(NA, 20)
for(i in 1:20){
  knn.mod = knn(trainx, testx, trainy, k = i)
  k.err[i] = 1-mean(knn.mod == testy)
  cat('Per k uguale a ', i, 'Errore = ', k.err[i], '\n')
}