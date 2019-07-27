#Exercice N°1
#1.a
library(datasets)
data<-airquality
data

#1.b
class(data)
names(data)
head(data)
dim(data)
str(data)
data$Ozone
data$Solar.R
data$Wind
data$Temp
data$Month
data$Day
attach(data)
Ozone
Month
#Data contient un tableau de données, sur la qualité de l'air de New York.

#1.c
mean(data$Temp)
quantile(data$Wind,1/5)

#1.d
hist(Temp)
hist(Temp, main="Histogramme des températures à New York", xlab="Température", ylab ="Nombre de Jour")

#2.a
tab<-anscombe
tab

#2.b
#tab contient les valeurs correspondant à anscombe, c'est à dire un tableau de données avec des moyennes, variances, coéficient de corrélation, droite de régression linéaire)

#2.c.d.e
attach(tab)
plot(x1,y1)
cor(x1,y1)
lm(y1~x1)
lin<-lm(y1~x1)
abline(lin)
#La droite de corrélation linéaire semble ne pas suivre tous les points du tableau.

plot(x2,y2)
cor(x2,y2)
lm(y2~x2)
lin<-lm(y2~x2)
abline(lin)
#La droite de corrélation linéaire ne passe par aucun point du nuage de point.

plot(x3,y3)
cor(x3,y3)
lm(y3~x3)
lin<-lm(y3~x3)
abline(lin)
#Ici les points semble mieux répartis.

plot(x4,y4)
cor(x4,y4)
lm(y4~x4)
lin<-lm(y4~x4)
abline(lin)
#Les points sont très mal répartis la droite de corrélation linéaire.

