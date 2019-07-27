#1
echp<-rpois (12000,3)
Y<- cumsum(echp)/1:12000
plot(1:12000,Y,type="l", lwd = 1, fg = "black", bty = "l",cex.lab = 1.5, cex.axis = 1,
     col.lab = "pink", col.axis = "red", main="Echantillon", ylab = "Frequence",
     xlab="Effectif")
abline(h=3,col="red")
#2
#La loi simulée est la loi de Poisson
#3
#Y représente les fréquences cumulées, dans le cadre d'un problème
#d'éstimation Y nous permet d'éstimer l'espérance.
#4
#Sur le graphique cela est bien représenté puisque la loi suivit tend vers 3, et la
#commande abline trace une droite qui en 3 qui est donc l'esperance.
#5
sim<-rbinom(10,100,0.01)
O<-cumsum(sim)/1:100
plot(1:100,O,type="l",lwd=1, fg="blue",cex.lab=1.5,cex.axis=1,col.lab="pink", 
     col.axis="green", main = "Echantillon binomial", ylab="Densité",xlab = "Effectif")
abline(h=0.2,col="red")

#Exercice
#1
N<-25000; n<-150; p<-0.4
x<-seq(-10,10,0.1)
eff<-(rbinom(N,n,p)-n*p)/sqrt(n*p*(1-p))
#2
mean(eff); var(eff)
#La moyenne est toujours proche de 0, et la variance est toujours proche de 1.

#3
bornes<-seq(-5.25,5.25,0.25)
hist(eff,freq=F,main="Histogramme des fréquences", xlab="",ylab="", col="blue",
     lwd="2", ylim = c(0,0.4))
#4
y<-dnorm(x)
lines(x,y,col="red")
#5
#Le graphique montre une approximation de la loi binomial par une loi normale
