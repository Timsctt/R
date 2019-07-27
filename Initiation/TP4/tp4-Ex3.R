#Exercice 3
#1.a
cake<-read.table("cake.csv",header = T,sep = ",")
head(cake)
str(cake)

#b.
cake$temperature
attach(cake)
class(temperature)
temperature<-as.factor(temperature)
class(temperature)

#c.
levels(temperature)<-c("T1","T2","T3","T4","T5","T6")
head(temperature)

#2.a
plot(cake$angle,main = "Angle de rupture des biscuit",
     ylab = "Niveau de rupture", xlab = "Effectif",cex.lab = 1.5,
     cex.axis = 0.9,col.lab = "pink", col.axis = "red",pch=4)
lm(angle~X)->lin
abline(lin,col="red")

#b.
plot(recipe,angle,range=0, boxwex=0.4,col="orange", 
     xlab="Type de recette",xlab="Angle",main="Rapport angle de rupture/recette",
     cex.lab=1.2,cex.axis=0.9,col.lab="green", col.axis="red",col.main="blue")
#La moyenne d'angle de rupture en fonction des recettes reste pour les trois recette
#cependant l'écart type des différentes recettes varie plus ou moins fortement.

#c.
plot(temperature,angle,range=0, boxwex=0.4,col="orange",xlab="Type de température",
     ylab="Angle de rupture",main="Rapport angle de rupture/température",cex.lab=1.2,
     cex.axis=0.9,col.lab="pink",col.main="blue")

#d.
pie.rep <- c(1,2)
names(pie.rep) <- c("angle de rupture >35 (24,4%)","angle de rupture ≤35 (79,6%)")
pie(pie.rep, col = c("red","blue"),main="% des répartitions d'angle de rupture ±35°")

#3
plot(angle[temp<200],main = "Angle de rupture des températures < 200",
     xlab = "Effectif",ylab = "Angle de rupture",cex.lab = 1.5, cex.axis = 0.9,
     col.lab = "violet",pch=4,col="navy blue",col.main="navy blue")
x<-X[1:135]
lm(angle[temp<200]~x)->lin
abline(lin)
plot(angle[temp>200],main = "Angle de rupture des températures > 200",
     xlab = "Effectif",ylab = "Angle de rupture",cex.lab = 1.5, cex.axis = 0.9,
     col.lab = "violet",pch=4,col="navy blue",col.main="navy blue")
y<-X[1:135]
lin2<-lm(angle[temp>200]~y)
abline(lm(lin2))

