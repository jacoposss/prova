#####Clustering

data('USArrests')

#####Analisi esplorativa####
apply(USArrests,2, mean)
apply(USArrests,2, sd)

summary(USArrests)


# le tecniche di clustering sono basate sulla distanza euclidea, quindi bisogna standardizzare
#####Standardizzazione####
df = scale(USArrests)

apply(df,2, sd)
apply(df,2, mean)



# kmeans parametri
#* il dataset (standardizzato)
#* centres   indica il numero di cluster
#* iter.max  indica il numero di iterazioni massime
#* nstart    indica il numero di volte che i centroidi iniziali vengono cambiati - di default è 1
#*           nstart=25  l'algoritmo proverà 25 configurazioni iniziali dei 4 centroidi 
#*           e seleziona la configurazione con total within sum of square minore

set.seed(123)
k.clu = kmeans(df, centers = 4, iter.max = 30, nstart = 25)

# se stampo il modello mi dice
#* numero di unità statistiche in ongi cluster
#* le medie nei cluster delle variabili
#* clustering vector dice a quale cluster appartiene ogni unità
#* restitusice anche la within cluster sum of square per ogni cluster 


# $cluster   ---> vettore dei cluster
k.clu$cluster

# $withinss ----> restituisce le within cluster sum of square
k.clu$withinss


k.clu$tot.withinss # somma delle within cluster sum of square


k.clu


USArrests$cluster = k.clu$cluster #

# con aggregate posso aggregare i dati in base a specifiche condizioni, calcolando statistiche riassuntive su gruppi di dati.
# al dataset originle aggiungo una colonna con i cluster (fatto sopra al cancelletto #)

# voglio calcolare le medie sul dataset non standardizzato
# gli passo il dataset originale
# by = list(USArrests$cluster)       aggrego le unità sulla base dei cluster (la variabile aggiunta al dataset)
# calcolo le medie

aggregate(USArrests, by = list(USArrests$cluster), mean)



####Plot dei cluster####
# per visualizzare i cluster posso anche usare la PCA ma è meglio usare factoextra


# la libreria factoextra serve a plottare i cluster
# funzione fvinz_cluster,    parametri---> i cluster (output clustering kmeans),  data= "il dataframe standardizzato"

library(factoextra)
fviz_cluster(k.clu, data = df)
# questa visualizzazione ci permette di vedere anche se i cluster si sovrappongono o no
# se non si sovrappongono ---> la clusterizzazione è buona


# fviz_dist permette prendendo come parametro le dist(nel dataframe) permette di plottare su una matrice simmetrica per vedere
# se si identificano i blocchi sulla diagonale
fviz_dist(dist(df))

# qui notiamo che i 4 cluster non sono ben identificabili dal grafico




####TWSS attraverso un loop####
# per vedere il numero di cluster migliore utilizzo la TWISS e creo un loop nella quale cambio il numero di cluster e prendo il migliore

twiss = NA

for(k in 1:20){
  ktemp = kmeans(df, centers = k, iter.max = 50, nstart = 25)
  twiss[k] = ktemp$tot.withinss 
}

# plotto per vedere sove si appiattisce la curva
plot(x = 1:20, y = twiss, xlab = 'Valori di k',
     ylab = 'TWSS', type = 'b')



# uso altri metodi per vedere il numero di cluster migliore

# si usa fviz_nbclust  ----> permette di visualizzare piu tecniche di validazione specificando il metodo
# dataset  --- la funzione usata per fare clusterign  --- il metodo di validazione  --- il numero massimo di cluster k.max


####Silhouette method####
fviz_nbclust(df, kmeans, method = 'silhouette', k.max = 20)

# piu la silouette è alta meglio è il numero dei cluster corrispondente
# spesso per k=2 la silouette è quella massima (non si conta quindi k=2)
# per questo motivo viene utilizzato poco come metodo

# si preferiscono elbow e gap



#####Elbow####
fviz_nbclust(df, kmeans, method = 'wss', k.max = 20)
# il migliore è il numero per cui la curva si appiattisce



####Gap statistic####
fviz_nbclust(df, kmeans, method = 'gap_stat', k.max = 20)
# il valore piu alto della gap è il migliore


##almeno 2 su 3 metodi devono esssere uguali per aver clusterizzato bene 
##ma difficilmente sono simili tra lori
## e quindi scegliamo sulla base di uno





## CLUSTERING GERARCHICO ##
# ci sono diverse funzioni per fare il clustering gerarchico

####HCLUST####
# prende due parametri:  la matrice di distanze e il metodo

# con dist creo la matrice di distanze   ----> prende la matrice e come default fa la distanza euclidea
d = dist(df)

h.clu = hclust(d, method = 'single')###Min
h.clu = hclust(d, method = 'complete')###Max     -----> di default è complete
h.clu = hclust(d, method = 'average')
h.clu = hclust(d, method = 'centroid')
h.clu = hclust(d, method = 'ward.D2')###Ward

####Dendrogramma
# si usa plot ---> clutering,  hang -1 indica che tutte le foglie partono uguali        cex specifica la grandezza 
plot(h.clu, hang = -1, cex=0.75) 

# con rect inserisco nel plot dei quadrati che ti identificano i cluster 
# oggetto      k numero cluster
rect.hclust(h.clu, k = 4)





####AGNES####   altro modo per fare clustering gerarchico
library(cluster)

# parametri: dataset e method
h.agnes = agnes(df, method = 'ward')

# tra gli elementi che stampa cìè $ac ---> agglomerative coefficient (piu è alto melgio è)

# l'agglomerative coeff permette di identificare quale metodo utilizzare
# questo perchè la principale difficoltà nel clustering gerarchioco non è identificare il numero di k
# ma capire quale metodo usare

# facciamo girare agnes con tutti i metodi per vedere il migliore
m = c('average', 'single', 'complete', 'ward')


#####Dendrogramma con agnes
pltree(h.agnes, hang = -1, cex=0.75)



#####Ottenere dei cluster####
# funzione cutree -----> oggetto clustering,  nmuemro di cluster (cut sta per tagliare)

cluster.hc = cutree(h.clu, k = 4)
USArrests$hclu = cluster.hc




####Confronto tra kmeans e hclust con k=4####
# per fare questo nel dataset di partenza mi creo due variabili:
# una che salva i cluster del kmeans e una che salva i cluster del gerarchico
#utilizzo la confusion matrix per vedere le differenze nella classificazione
set.seed(123)

d1 = dist(df)
h1 = hclust(d1, method = 'ward.D2')
k1 = kmeans(df, centers = 4, iter.max = 50, nstart = 25)

USArrests$Hclust = cutree(h1, k = 4)
USArrests$Kmeans = k1$cluster

table(kmeans = USArrests$Kmeans, hclust = USArrests$Hclust)



#####Taglio con quantile#####
# per tagliare il dendogramma con le distanze 
# si utilizza l'elemento $height restituisto dal clustering

h.clu$height

# trovo l'altezza media, lo passo a cutree cn il parametro h e taglio
meantaglio = mean(h.clu$height)
cluster.hc2 = cutree(h.clu, h = meantaglio)
table(cluster.hc2)

# posso anche ricavare il quantile e poi tagliare il quantile 
# (di solito si usa taglio 80)
taglio80 = quantile(h.clu$height, 0.80)
cluster.hc3 = cutree(h.clu, h = taglio80)
table(cluster.hc3)

taglio95 = quantile(h.clu$height, 0.95)
cluster.hc4 = cutree(h.clu, h = taglio95)
table(cluster.hc4)



####Confrontare due dendrogrammi####
hc1 = hclust(d, method = 'ward.D2')
hc2 = hclust(d, method = 'complete')

dend1 = as.dendrogram(hc1)
dend2 = as.dendrogram(hc2)

library(dendextend)
tanglegram(dend1, dend2)


