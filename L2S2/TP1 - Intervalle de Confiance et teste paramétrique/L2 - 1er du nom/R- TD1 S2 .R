#Exercice 1
#1
m<-2; s<-1
n<-100; N<-200
X<-matrix(rnorm(n*N,m,s),n,N)

#2
Xbar<-colSums(X/n)
plot(Xbar, 1:N, xlim=c(1,3), main ="")

#3
Ibinf<-Xbar-1.96*s/sqrt(n)

#4
Ibsup<-Xbar+1.96*s/sqrt(n)

#5
sum((m>Ibinf)&(m<Ibsup))
#Le nombre d'échantillon tel que l'intervalle de confiance contient l'espérance

#6
sum((m>Ibinf)&(m<Ibsup))/N
#La proportion d'intervalle de confiance contenant la valeur de l'espérance de 97%

#7
segments(Ibinf, 1:N, Ibsup, 1:N)

#8
a<-0.5
Ibinf2<-Xbar-qnorm(1-a/2)*s/sqrt(n)
Ibsup2<-Xbar+qnorm(1-a/2)*s/sqrt(n)
sum((m>Ibinf2)&(m<Ibsup2))
sum((m>Ibinf2)&(m<Ibsup2))/N
segments(Ibinf2, 1:N, Ibsup2, 1:N)

#9
a<-0.05 
Z<-qnorm(1-a/2)

#On change la taille de l'échantillon "n"

#Exercice 2
#1
prix1<-c(42.7,42.6,43,43.5,42.8,42.1,43.6,42.9,41.6,42.8,42.9,43.2,42.6,43.1,43.1)

#2
hist(prix1)
boxplot(prix1)

#3
shapiro.test(prix1)

#Test de Shapiro-Wilk : teste si un échantillon suit une loi normale ou non 
#(hypothèse nulle : suit une loi normale, donc si p-value < 0.01, l'échantillon ne 
#suit pas une loi normale). Cela permet alors d'utiliser certains tests statistiques 
#si la réponse est oui.
#Donc ici le test suit une loi normale. W est la variable aléatoire de test.

#4
t.test(prix1, mu=43,alternative = "two.sided")
#two.sided = deux côté, si on ne spécifie pas alternative il considere bilateral

#5
#df: degre de liberte
#t : student
#p-value : a valeur p est la probabilité d'obtenir la même valeur du test 
#si l'hypothèse nulle était vraie

#6
test<-t.test(prix1, mu=43, alternative = "two.sided")
test$p.value;test$conf.int;test$estimate;test$null.value;test$alternative;test$method

#7
test$estimate
test$conf.int

#8
test<-t.test(prix1, mu=43, alternative = "two.sided", conf.level=0.9)$conf.int

#9
#a
prix2<-c(40.9,43.4,42.2,41.6,42.9,40.8,42.5,41.6,40.4,42.5,41.1,41.8,43.3,43.4,43.4)
#b
boxplot(prix1,prix2)
#c
var.test(prix1,prix2)
#d
t.test(prix1,prix2,alternative = "greater",var.equal=FALSE)
