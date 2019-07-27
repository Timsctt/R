#Erxecice 1
#1
dbinom(3,10,0.5)
dbinom(-1,10,0.5)
loibin<-dbinom(0:10,10,0.5)
loibin
cumsum(loibin)
pbinom(5,10,0.5)
rbinom(2,10,0.5)

#2
plot(rbinom(15,10,0.5))
plot(rbinom(15,10,0.5), main="Lancé de 15 variable aléatoire", xlab="Effectif", ylab ="Variable aléatoire")

#3
par(mfrow=c(1,3))
plot(table(rbinom(30,10,0.5)), main="Tableau", ylab ="Effecif", xlab="Nombre")
plot(table(rbinom(50,10,0.5)), main="Tableau", ylab ="Effecif", xlab="Nombre")
plot(table(rbinom(300,10,0.5)), col="blue", lwd = "30", main="Tableau", ylab ="Effecif", xlab="Nombre")

#Exercice 2
x<-seq(-5,5,0.1); y<-seq(0,1,0.01); par(mfrow=c(1,2))
plot(x,dnorm(x,0,1),type = "l", main="densité", xlab = "",ylab = "")
plot(x,pnorm(x,0,1),type = "l", main="fonction de répartiton", xlab = "",ylab = "")
pnorm(0,0,1)
qnorm(0.975)
qnorm(0.95)
hist(rnorm(1000,5,2.5),freq=F, main="échantillons", xlab="",ylab="", col="blue", lwd="2", xlim=c(-5,15))#, ylim=c(0,0.16))
lines(x,dnorm(x,5,5),col="red")

#exercice 3
