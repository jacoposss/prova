#* usa i dati WAGE2 nella lbreria woolrdidge
#* stima il modello educazione su sibs, meduc, fedc

library(wooldridge)
data(wage2)

?wage2

summary(wage2)

modello <- lm(educ ~ sibs+meduc+feduc, data=wage2)
summary(modello)

#* all'aumentare di un fratello o sorella gli anni medi di istruzione diminuiscono di 0,09364
#* il coeffixiente è negativo perchè se aumenta il numero di figli il budget previsto per la loro istruzione deve essere diviso
#* in piu parti
#* all'aumento di un anno di istruzione della madre gli anni di istruzione medi aumentano di 0,13
#* all'aumento di un anno di istruzione del padre gli anni di istruzione medi aumentano di 0,21 
#* entrambi i coefficienti sono positivi perchè i genitori con un'istruzione "piu alta" molto probabilmente tengono particolarmente
#* all'istruzione del figlio e gli danno piu importanza
#* 
#* l'Rquadro è molto basso, il modello spiega circa il 21% della y
#* 
#* tutti i coefficienti sono statisticamente significati con una size del test di 0,05

# intervalli di confidenza
confint(modello, level=0.95)
# nessun intervallo contiene lo zero, anche da qui si capisce che sono tutti statisticamente significativi

# intervalli di previsione
predict(modello, data.frame(sibs=c(1,2,3), meduc=c(6,12,11), feduc=c(2,4,9)), interval = 'prediction')
