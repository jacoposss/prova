
print("hello world")

4*2+1

sqrt(9)

abs(-22)

sqrt(c(3,6,9,19,10))

?abs

#Vettori
y <- 6
y

z = rnorm(1000)
z
head(z) # mostra i primi sei valori

remove(z) # rimuovere oggetto

class(y)

x <- c(1,2,4,8)
x
length(x)

nomi <- c('Andrea', 'Paola', 'Pippo', 'Franco')
length(nomi)

class(nomi)

x1 <- c(5,6,6,8)

vet <- c(1,2,3,4,"ciao")
class(vet)

z =2
x+x1

z + x1

x1 + c(1,2)


mat <- matrix(10:15, nrow=2, ncol=3) # riempie la matrice per colonna 
mat
class(mat)

mat2 <- matrix(10:15, nrow=2, ncol=3, byrow=TRUE)# per riempire la matrice per riga parametr byrow = TRUE
mat2
dim(mat2)


x <- 1:3
y <- 7:9

m1 <- cbind(x,y)
m1

m2 <- rbind(x,y)
m2

convertita <- as.data.frame(m2)
class(convertita)

miodataframe <- data.frame(a=1:4,sesso=c('M', 'F', 'F', 'M'))
class(miodataframe)

miodataframe


miodataframe[1:3,]

miodataframe[,1]

miodataframe$sesso

attributes(miodataframe)

dim(miodataframe)


datatagliato <- miodataframe[c(1,3), 2]
datatagliato




##Funzioni e operazioni
sqrt(9)
sqrt(x)
x^2

# creare funzione

media <- function(x){
  return(mean(x))
}

media(5)


###Area di lavoro
ls()
objects()
rm(list=ls())


save.image("myWspace.RData")

getwd()

setwd("C:/Users/andre/Documents/Rworks")

##Pacchetti
install.packages('xlsx')

library(xlsx)

vignette('xlsx')

data()



##Help

?length
help(length)
help.start()


##Alcune funzioni utili
x2 = rnorm(50)

y = x2 + rnorm(50, mean = 50, sd = .1)

cor(x2, y)

set.seed(1503)
rnorm(50)


mean(y)
sd(y)

seq(1, 10)
1:10

seq(1, 10, length = 50)

##Grafici
plot(x2, y)

?plot
plot(x2, y, main = 'Grafico a dispersione si X2 vs Y', xlab = "Qui va l'etichetta dell'asse x", 
     ylab = "Qui va l'etichetta dell'asse y")


hist(y)

pdf('Figure lab.pdf')
plot(x2, y, main = 'Grafico a dispersione si X2 vs Y', xlab = "Qui va l'etichetta dell'asse x", 
     ylab = "Qui va l'etichetta dell'asse y")
dev.off()

x = seq(-pi, pi, length = 50)
y = x

?outer
f = outer(x, y, function(x, y)cos(y)/(1+x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)

fa=(f-t(f))/2
image(x, y, fa)
persp(x, y, fa)

##Leggere i dati in R

data = read.table(file = 'clipboard', sep = '\t', header = TRUE)
data = read.csv('C:/Users/Jacopo/Desktop/data mining/regressione/Advertising.csv', dec = '.')


fix(data)
dim(data)


data$TV = as.numeric(data$TV)