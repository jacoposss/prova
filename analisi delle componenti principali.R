####Analisi delle Componenti Principali####
data("mtcars")

# l'analisi delle componenti principali non puo essere applicata a variabili qualitative
# creo un sotto-set escludendo le qualitative x fare la PCA

mtcars <- mtcars[,1:7]


# PRIMO STEP: analisi esplorativa dei dati
##Media
apply(mtcars, 2, mean)

##Deviazione standard
apply(mtcars, 2, sd)

# ha un peso piu alto la variabile con varianza (o dev standard) PIU ALTA



# SECONDO STEP: standardizzare

####Opzione 1 per standardizzare####
# prima standatdizzo il dataset e poi faccio le componenti principali
# per standardizzare uso apply per scalare ogni colonna
mtcars_scaled = apply(mtcars, 2, scale)
mtcars_scaled


# prcomp permette di fare la PCA parametri ----> dataset scalato,    center = FALSE
PCA1 = prcomp(mtcars_scaled, center = FALSE)

summary(PCA1)

biplot(PCA1, scale=0)

####Opzione 2 per standardizzazione####
# fai standardizzazione direttameneìte in prcomp (datasetDaScalare,    scale. = TRUE  ti fa la standardizzzione direttamente)

PCA2 = prcomp(mtcars, scale. = TRUE)
PCA2



##Deviazioni standard delle Z
PCA2$sdev

##matrice Factor loading
PCA2$rotation

##Medie delle X originali
PCA2$center

##Deviazioni standard delle X originali
PCA2$scale

###Z - componenti principali
PCA2$x

####
summary(PCA2)
# con il summary vediamo limportanza delle componeiti:
# indica la deviazione standard, la porzione di varianza spiegata di ogni componente e la cumulativa


# BIPLOT
biplot(PCA2, scale=0) # gli passo le componenti principali

# le variabili piu importanti sono quelle nel quadtante in alto a destra nel plot
# perchè hanno entrambi i factor loading positivi

# DI SOLITO SI CONIDERANO OUTLIERS I PUNTI FUORI DALLE CORDINATE 2, -2 DI TANTO
