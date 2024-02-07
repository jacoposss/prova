Advertising = read.csv('C:/Users/Jacopo/Desktop/data mining/regressione/Advertising.csv', dec = '.')

# la prima colonna è inutile: creo un ogetto con lo stesso nome avente la prima colonna eliminata
Advertising = Advertising[,-1] 

mean(Advertising$Sales) # media vendite

# summary mostra alcune statistiche di base: minimo e massimo valore assunto, media, mediana 1' e 3' quartile
summary(Advertising) 

var(Advertising$Sales) # varianza vendite
sd(Advertising$Sales) # deviazione standard vendite

# permette di applicare una funzione a tutte le righe o colonne di un oggetto
# parametri (oggetto, 1 se per righe 2 se per colonne, funzione)

apply(Advertising, 2, mean) # media per colonna
apply(Advertising, 2, sd) # deviazione standard per colonna

# visualizzo le correlazioni per vedere se qualche regressore è correlato alla dipendente
# o se c'è il problema della multicollinearità (regressori corellati tra loro)

cor(Advertising) # matrice delle correlazioni


# tv e radio hanno una correlazione con la dipendente sales sopra 0,50
# i regressori non sono altamente correlati tra loro (la piu alta è tra radio e newspaper 0,35)



## VISUALIZZAZIONI GRAFICHE ##
# il comando plot restituisce un grafico a dispersione
# argomenti (variabile x , variabile y, label x, label y, label grafico)

plot(x = Advertising$TV, y = Advertising$Sales, 
     xlab = 'Spesa per TV', ylab = 'Vendite prodotto',
     main = 'Relazione tra spesa TV e vendita')





####REGRESSIONE LINEARE SEMPLICE####
slr.fit = lm(Sales~TV, data = Advertising) 

summary(slr.fit)
# al crescere di un unità della varaibile spesa per pubblicità in televisione le vendite aumentano di 0,047(unit mis milioni)
# L'Rquadro è 0,61, cio singifica che il modello spiega il 61% delle vendite

# entrambi i coefficienti sono statisticamente singifcativi


# intervalli di confidenza: confint(modello, level = size)

confint(slr.fit, level = 0.95) # l'intervallo di TV non contiene lo zero, quindi il coeffiente è statisticamente singif.

confint(slr.fit, level = 0.99)


# FARE PREVISIONE
# comando predict
# 3 argomenti: 
       # 1 il modello 
       # 2 data.frame (creo un data frame con i valori con cui fare previsione) 
       # 3 parametro interval = "prediction" (indica che vogliamo trovare gli intervalli di previsione) 
       # 4opzionale level (indica l'ampiezza dell'intervallo, di defualt 95%)

predict(slr.fit, data.frame(TV = c(100,150,200)),
        interval = 'prediction')
# risultato: fit-> valore della y predicted   lwr e upr-> estremi inferiore e superiore dell'intervallo di previsione


# con abline aggiungo una retta nel plot
plot(x = Advertising$TV, y = Advertising$Sales, 
     xlab = 'Spesa per TV', ylab = 'Vendite prodotto',
     main = 'Relazione tra spesa TV e vendita')
abline(slr.fit, col = 'red')


####REGRESSIONE LINEARE MULTIPLA####
mlr.naif = lm(Sales ~ TV + Radio + Newspaper, data = Advertising) 

# il punto indica di prendere come regressori tutte le variabili ad esclusione della dipendente+
# lm(Sales ~ . , data = Advertising) 

summary(mlr.naif)
# ad un aumento unitario della variabile spesa per pubblicità in televisione le vendite aumentano di 0,04(unit mis milioni)
# ad un aumento unitario della variabile spesa per pubblicità in radio le vendite aumentano di 0,18(unit mis milioni)
# ad un aumento unitario della variabile spesa per pubblicità su giornale le vendite dimiuiscono di 0,001(unit mis milioni)

# l'unico coefficiente non statisticamente singificativo è newspaper

# L'Rquadro è 0,89, cio singifica che il modello spiega l'89% delle vendite

confint(mlr.naif)


# per fare previsione indico i valori per ogni regressore nel data.frame
predict(mlr.naif, data.frame(TV = c(100,150), Radio = c(15,30),Newspaper = c(50,50)),
        interval = 'prediction')




# togliamo newsaper che non è significativo

mlr.fit2 = lm(Sales ~ TV + Radio, data = Advertising)
summary(mlr.fit2)

# facendo cio ma migliora la statistica F enormemente 
# (cio indica che newspaper era inutile e i due regressori tv e radio sono essenziali)

confint(mlr.fit2)




### TERMINE QUADRATIVO ###

# per inserire una forma quadratico uso la funzione I prendendo tra parametri il termine alla potenza

mlr.sq = lm(Sales ~ TV + I(TV^2)  + Radio, data = Advertising)
summary(mlr.sq)

# da questo modello capiamo che se aumentassi esponenzialmente la pubblicità in tv le vendite diminuirebbero


# poly permette di aggiungere alla regressione tutti i polinomi fino al grado indicato come parametro 
# poly(Tv,3) aggiunge tv, tv^2, tv^3

mlr.fit3 = lm(Sales ~ poly(TV,3) + Radio, data = Advertising)
summary(mlr.fit3)



### TERMINE LOGARITMICO ###

# con log inserisco il logaritmo

mlr.log = lm(Sales ~ TV + I(TV^2) + log(Newspaper), data = Advertising)
summary(mlr.log)




####REGRESSORE QUALITATIVO#####

# mi creo la variabile qualitativa

# il comando cat permette di tagliare una variabile quantitativa

# taglio la spesa per pubblicità in radio in due categorie: bassa - alta

Advertising$Radiocat = 'Bassa'
Advertising$Radiocat[Advertising$Radio > mean(Advertising$Radio)] = 'Alta'
Advertising$Radiocat = as.factor(Advertising$Radiocat)

mlr.fit4 = lm(Sales ~ TV + Radiocat, data = Advertising)
summary(mlr.fit4)

# con una spesa alta in radio le vendite sono circa 9,62
# con una spesa alta in radio le vendite sono circa 4,83
# con una spesa bassa per pubblità in radio si hanno vendite medie piu basse di 4,79 rispetto a quelle con un spesa alta per pubblicità


Advertising$Radiocat2 = 'Bassa'
Advertising$Radiocat2[Advertising$Radio >= 10 & 
                        Advertising$Radio < 25] = 'Media'
Advertising$Radiocat2[Advertising$Radio >= 25] = 'Alta'
table(Advertising$Radiocat2)
Advertising$Radiocat2 = as.factor(Advertising$Radiocat2)

mlr.fit5 = lm(Sales ~ TV + Radiocat2, data = Advertising)
summary(mlr.fit5)