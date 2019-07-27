#Exercice 1

#1
#A

n<-20
m<-5
s<-1
test1<-t.test(rnorm(n, m, s), mu=4.5)
test1

#B
m<-5
s<-1
n<-20
N<-100
rejet<-rep(0,100)
for(i in 1:100) {
  testencours<-t.test(rnorm(n,m,s),mu=4.5)
  rejet[i]<-testencours$p.value<0.05
}
  nbrejet<-sum(rejet)
  nbrejet/100
  
#C
  m<-5
  s<-1
  n<-100
  N<-100
  rejet<-rep(0,100)
  for(i in 1:100) {
    testencours<-t.test(rnorm(n,m,s),mu=4.5)
    rejet[i]<-testencours$p.value<0.05
  }
  nbrejet<-sum(rejet)
  nbrejet/100
  
  
#2
#A
power.t.test(n=20,delta=0.5,sd=1,sig.level=0.05,type="one.sample",alternative="two.sided")

#B
#n<-nbr d'échantillons
#delta=Marge d'erreur accepté en fonction d'H0
#sd=écart type
#sig.level=Le  risque du test


power.t.test(n=5,delta=0.5,sd=1,sig.level=0.05,type="one.sample",alternative="two.sided")

#C
power.t.test(n=100,delta = 0.5,sd=1,sig.level = 0.05,type="one.sample",alternative = "two.sided")

#La puissance du test est de 0.998

#D
power.t.test(delta=0.5,sd=1,sig.level=0.05,power=0.8,type="one.sample",alternative="two.sided")

#indique le nombre d'échantillon  minimum pour répondre aux critères d'avoir une marge 
#d'erreur de 0.5, un écart type de 1, d'une puissance de test de 80%


#############
#Exercice 2 #
#############

#1
power.t.test(sd=1.7,sig.level=0.05,delta=1,n=20,type="two.sample",alternative="two.sided")

#2
power.t.test(sd=1.7,sig.level=0.05,power=0.8,n=20,type="two.sample",alternative="two.sided")

#3
power.t.test(sd=1.7,sig.level=0.05,power=0.8,delta=1,type="two.sample",alternative="two.sided")


###########
#Exercice 3
###########

#1
#A
prop.test(18,64,0.15)
binom.test(18,64,0.15)

#B
test<-binom.test(18,64,0.15,conf.level = 0.96)$conf.int
test
#l'intervalle de confiance au seuil de 96% est de [0.17;0.41]

#C
prop.test(18,64,0.15,alternative="greater")
#La p-value est plus petite donc on rejette l'hypothèse H0 ce qui signifie la proportion 
#de gauchers est plus petite que la population globale

#2
adherents<-read.table("adherents.txt",header=T,sep=";",row.names=1)
#A
nbrG<-sum(adherents$lateralisation=="G")
prop.test(nbrG,nrow(adherents),0.15)
binom.test(nbrG,nrow(adherents),0.15)
#La probabilite d'obtenir un gaucher est de  0.176

#B
table(adherents$lateralisation,adherents$sexe)
prop.test(table(adherents$lateralisation,adherents$sexe))

#C



#3
power.prop.test(n=125, p1=0.4, p2=0.5)
power.prop.test(p1=0.4, p2=0.5, power = 0.85)
power.prop.test(n=125, p2=0.5, power = 0.9)