# esercizio libro econometria pag 199 - c2 (senza logaritmo)

library(wooldridge)
data("wage1")

summary(wage1)
?wage1

modello = lm(wage ~ educ+exper+I(exper^2), data=wage1)
summary(modello)

confint(modello, level=0.95)
confint(modello, level=0.99)

# tutti i coefficienti sono statisticamente singificativi sia al 5% che all'1%

# un po di previsioni
predict(modello, data.frame(educ=c(11,11,7), exper=c(1,3,8)), interval = 'prediction')

