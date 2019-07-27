#Exercice 1
#1
library(datasets)
iris->data

#2
class(data)

#3
str(data)

#4
summary(data)

#5
G2<-data[data$Species=="versicolor",]

#6
boxplot(G2$Sepal.Length,horizontal = T, main="Répartition de l'espace Versicolor", 
        xlab="Longueur des sépales")

#7
mean(data$Petal.Length[data$Species=="setosa"])

#8
sum(data$Sepal.Length>=6)/nrow(data)


#Exercice 2
#1
read.table("DonneesCCL2R.txt",sep="\t",dec=".",header=T)->tab

#2
str(tab)

#3
vec<-rep(1,nrow(tab))
vec[tab$Trans=="Arrière"]<-0
tab$Trans<-vec

#4
tab[tab$Prix>=20000&tab$Puissance<=150,]

#5
tab2<-tab[-c(1,2,5,6,9)]
tab2

#6
cor(tab2)
#Les corrélation possible sont Prix/Puissance et Prix/Poids

#7
plot(tab$Poids,tab$Prix,main="Regression lineaire Prix/Poids",xlab="Poids",ylab="Prix")
abline(lm(tab$Prix~tab$Poids),col="red")