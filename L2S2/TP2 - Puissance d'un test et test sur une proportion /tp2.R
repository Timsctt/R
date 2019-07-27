#Exercice 1
#1
#a).
n<-20; m<-5; s<-1
test1<-t.test(rnorm(n, m, s), mu=4.5); test1

#b).
m<-5; s<-1; n<-100; N<-100
rejet<-rep(0,100) 
for (i in 1:100) {
  testencours<-t.test(rnorm(n,m,s), mu=4.5)
  rejet[i]<-testencours$p.value<0.05
}
nbrejet<-sum(rejet);nbrejet/100

#2
#a).
power.t.test(n=20,delta = 0.5,sd=1,sig.level = 0.05,type="one.sample",
             alternative = "two.sided")
#power = 56% de chance de reussir a detecter un ecart avec un echantillon de 20

#b).
# n=taille de l'echantillon; sd=ecart type; delta=ecart a l'esperance
#sig.level=signification du niveau, type d'erreur possible

#c).
power.t.test(n=100,delta = 0.5,sd=1,sig.level = 0.05,type="one.sample",
             alternative = "two.sided")

#d).
power.t.test(delta = 0.5,sd=1,sig.level = 0.05, power=0.8,type="one.sample",
             alternative = "two.sided")
#indique le n minimum



#Exercice 2
#1
power.t.test(n=20,delta =1,sd=1.7,sig.level = 0.05,type="two.sample",
             alternative = "two.sided")

#2
power.t.test(n=20,power=0.8,sd=1.7,sig.level = 0.2,type="two.sample",
             alternative = "two.sided")

#3
power.t.test(power=0.8,delta =1,sd=1.7,sig.level = 0.05,type="two.sample",
             alternative = "two.sided")




#Exercice 3
#1
#a).
prop.test(18,64,0.15)
#test approche
binom.test(18,64,0.15)
#test exact

#b).
binom.test(18,64,0.15)->test
test$conf.int

binom.test(18,64,0.15,conf.level = 0.96)$conf.int

#c).
binom.test(18,64,0.15,alternative = "greater")

#2
#a).
adherents<-read.table("adherents.txt",header = T,sep = ";",row.names = 1)
sum(adherents$lateralisation=="G")


prop.test(sum(adherents$lateralisation=="G"),nrow(adherents),0.15)
binom.test(sum(adherents$lateralisation=="G"),nrow(adherents),0.15)

#b).
table(adherents$lateralisation,adherents$sexe)
prop.test(table(adherents$lateralisation,adherents$sexe))

#c).
#La p-value grande on rejette donc H0, H0 est donc vrai. On fait un test d'egalite
#de proportion. Il n'y a pas de différence de proportion de gaucher chez les femmes
#et chez les hommes.

binom.test(sum(adherents$sexe=="H"),nrow(adherents),0.5)

#3
power.prop.test(n=125, p1=0.4, p2=0.5)
power.prop.test(p1=0.4, p2=0.5, power = 0.85)
power.prop.test(n=125, p2=0.5, power = 0.9)

