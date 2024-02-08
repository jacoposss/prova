# Si utilizzi il dataset “Boston” per stimare la relazione tra il valore medio di una casa (medv, variabile dipendente) in $1000, 
# il numero di crimini pro capite nella zona (crim), la vicinanza al fiume Charles (chas, 1 se in riva al fiume, 0 diversamente) 
# e il numero medio di camere (rm). Si incolli qui sotto l’output della stima.

library(MASS)
data(Boston)

?Boston

summary(Boston)

modello = lm(medv~crim+chas+rm, data=Boston)
summary(modello)



# a. Si commenti il risultato ottenuto nel punto precedente, 
# dando una spiegazione esaustiva sul risultato riguardante il segno dei coefficienti e la loro significatività.

#* all'aumentare di un rate del crimine il prezzo medio delle case dinimuisce di 260$ tenendo gli altri fattori fissi
#* se la casa è in riva al fiume il prezzo medio delle case è 3760$ piu altro rispetto alle case che non lo sono tenendo gli altri fattori fissi
#* le case che non sono vivino al fiume hanno un prezzo medio piu basso di 28810$tenendo gli altri fattori fissi
#* all'aumentare di una camera in casa il prezzo medio della case aumenta di 8278$tenendo gli altri fattori fissi
#* 
#* i coefficienti sono tutti statisticamente significativi perhcè i p-value sono piu piccoli di 0,05 (e le t-stat in valore assoluto >2)
#* 
#* il segno di crim è negativo perchè all'aumentare del crimine diminusce la domanda delle case e di consegueza scendono i prezzi
#* il coefficiente di rm è positivo perchè se aumentano il numero di stanze (probabilmenta aumenta anche la dimensione della casa)
#* e di conseguenza sale il prezzo
#* il segono di chas è positivo perchè la vicinanza al fiume potrebbe indicare la presenza di servizi aggiuntivi o che la casa sia fuori città
#* e di dimensioni piu grandi
#* l'intercetta è negativa perchè indica la lontanza dal fiume e di conseguenza l'assenza dei benefici nello stargli vicino


# b. Si preveda la variabile dipendente utilizzando i coefficienti stimati in precedenza sulla base dei seguenti valori dei regressori. 
# crim = 3   chas = 0  rm = 5
# crim = 2  chas = 1  rm = 4
# crim = 0.1  chas = 1  rm = 5
# Si riportino anche gli intervalli di previsione al 95%.

previsioni = predict(modello, data.frame(crim=c(3,2,0.1), chas=c(0,1,1), rm=c(5,4,5)),
                     interval = "prediction")

previsioni
