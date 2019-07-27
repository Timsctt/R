#Exercice 4
films<-read.table("films.txt",header = T,sep = ";")
attach(films)

#1
sd(rating)

#2
power.t.test(power=0.9,delta = 0.5,sd=1.551333,sig.level = 0.01,type="one.sample",
             alternative = "two.sided")
#Il suffit d'avoir un echantillon de 146.5699, soit 147 films

#3
power.t.test(power=0.9, delta = 0.5 ,sd=1.551333,sig.level = 0.01,type="two.sample",
             alternative = "two.sided")
#Un echantillon de 289 films est necessaire pour avoir un ecart de note de 0.5.

#4
sum(budget<100000&rating>8)
# 103 films on une note superieur ou egale a 8 avec un budget inferieur a 100 000
#dollars
sum(budget<100000)

binom.test(94,364,0.07,alternative="less")
#On rejette l’hypothèse nulle donc on considère l’alternative vraie.

#5
power.prop.test(n =400, p2=0.07, power =0.9)
#Au seuil de 7%, dans un échantillon de taille 400, l’écart par rapport à 0.07 
#détectable dans 90% des cas est de 0.22.

#6
grosbudget<-films$budget>100000
petitbudget<-films$budget<100000
t.test(grosbudget,petitbudget, type="two.sample")
#On rejette l’hypothèse nulle donc on peut comprendre que les notes des films n'ont
#de liens avec leur budget.