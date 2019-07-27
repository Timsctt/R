#Exercice 3
#1
X<-airquality
attach(X)
n<-30.6; N<-5; a<-0.1
s<-sd(Wind)
Xbar<-mean(Wind)
Ibinf<-Xbar-qnorm(1-a/2)*s/sqrt(n)
Ibsup<-Xbar+qnorm(1-a/2)*s/sqrt(n)
#L'intervalle de confiance à l'ordre de 90% est [8,91;11,005]

#2
windy<-rep(1,0)
windy[Wind>=10]<-1
windy[Wind<10]<-0

#3
plot(Ozone,Wind,main = "Rapport Ozone/Vent",xlab = "Ozone",ylab = "Vent",
     col.main="Blue",cex.lab=1.2,cex.axis=0.9,col.lab="green", col.axis="30")
abline(h=10,col="red")
#Apres la ligne rouge representant la valeur du mph ou "Wind" dépasse 10, on peut 
#voir que les valeur "Ozone" sont significativement inférieur à celle avant la ligne 
#rouge

#4
t.test(Ozone, mu=40,alternative = "two.sided")
#On peut dire que la concentration d'ozone à New York est environ la même, puisqu'elle
#est de 42.12931, donc seulement 2.12931 de plus. Il est dans l'intervalle de 
#confiance à 95% donc on rejette l'hypothese que la concentration d'ozone est
#similaire.



#Exercice 4
n1<-2000; N1<-1; a1<-0.02
films<-read.table("films.txt",header = T,sep = ";")
#1
attach(films)
hist(length,ylab = "Effectif",xlab="Durée",
     main = "Effectif des films selon leur durée",col.main="blue",col="orange",
     cex.lab=1.2,cex.axis=0.9,col.lab="51", col.axis="550")

hist((budget/100)[length<100],ylab = "Effectif",xlab="Budget/100",
     main = "Effectif des films selon le budget consacrée",col.main="blue",
     col="orange",cex.lab=1.2,cex.axis=0.9,col.lab="51", col.axis="550")
hist((budget/100)[length>100],ylab = "Effectif",xlab="Budget/100",
     main = "Effectif des films selon le budget consacrée",col.main="blue",
     col="orange",cex.lab=1.2,cex.axis=0.9,col.lab="51", col.axis="550")

#2
Xbar1<-mean(budget)
s1<-sd(budget)
Ibinf<-Xbar1-qnorm(1-a1/2)*s1/sqrt(n1)
Ibsup<-Xbar1+qnorm(1-a1/2)*s1/sqrt(n1)
#On obtient un intervalle de confiance de 98% egale a [11680878;14057133]

#3
t.test(rating[length>150], mu=6,alternative = "two.sided")
#On rejette l'hypothèse nulle, ce qui signifie que les films d'une duree egale ou
#suprerieur a 2h30 ont une note superieur a la moyenne de 6.

#4
budget[budget>=1000000]<-1
grosbudget<-budget

#5
t.test(rating[grosbudget==1], mu=6,alternative = "two.sided")
#On rejette l'hypothèse nulle, ce qui signifie que les films à gros budget n'ont 
#pas d'incidence sur l'avis du public
plot(rating[grosbudget==1],main = "Disparité des notes des films",xlab = "Effectif",
     ylab = "Note",col.main="blue",col.axis="65",cex.lab=1.2,cex.axis=0.9,
     col.lab="51")