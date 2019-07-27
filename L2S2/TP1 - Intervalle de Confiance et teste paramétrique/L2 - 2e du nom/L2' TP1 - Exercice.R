m<-2
s<-1
n<-100
N<-200
X<-matrix(rnorm(n*N,m,s),n,N)
X
#2
Xbar<-colSums(X/n)
Xbar
plot(Xbar, 1:N, xlim=c(1,3))

#3
Ibinf<-Xbar-1.96*s/sqrt(n)
Ibinf

#4
Ibsup<-Xbar+1.96*s/sqrt(n)
Ibsup

#5
sum((m>Ibinf)&(m<Ibsup))
#la proportion de m qui est dans l'intervalle de confiance

#6
sum((m>Ibinf)&(m<Ibsup))/N
#94% d'intervalle contient l'esperance

#7
segments(Ibinf,1:N,Ibsup,1:N)
abline(v=2,col="red")

#8
a<-0.1
qnorm(1-a/2)
Ibsup2<-Xbar+qnorm(1-a/2)*s/sqrt(n)
Ibinf2<-Xbar-qnorm(1-a/2)*s/sqrt(n)

segments(Ibinf2,1:N,Ibsup2,1:N)



#9
a<-0.05
n<-4000
Ibsup<-Xbar+qnorm(1-a/2)*s/sqrt(n)
Ibinf<-Xbar-qnorm(1-a/2)*s/sqrt(n)
segments(Ibinf,1:N,Ibsup,1:N)


#Exercice2
#1
prix1<-c(42.7,42.6,43,43.5,42.8,42.1,43.6,42.9,41.6,42.8,42.9,43.2,42.6,43.1,43.1)

#2
hist(prix1,col="red")
boxplot(prix1,col="orange")

#3
shapiro.test(prix1)
#Ce test sert à savoir si un echantillon suit une loi normale. Si p-value < 0.01
#l'échantillon ne suit pas une loi normale. Ici, il suit donc une loi normale

#4
t.test(prix1, mu=43, alternative="two.sided")

#5
#t : student
#df : degré de liberté
#p-value : On compare la p-value au niveau de confiance pour savoir si 
#l'hypothese vraie est accepte. C'est la valeur d'acceptation du test.

#6
test<-t.test(prix1, mu=43, alternative="two.sided")

test$p.value
test$conf.int
test$estimate
test$null.value
test$alternative
test$method

#7
test$estimate
test$conf.int

#8
test<-t.test(prix1, mu=43, alternative = "two.sided", conf.level=0.9)$conf.int

#9
prix2<-c(40.9,43.4,42.2,41.6,42.9,40.8,42.5,41.6,40.4,42.5,41.1,41.8,43.3,43.4,43.4)

boxplot(prix1,prix2)

var.test(prix1,prix2)

t.test(prix1,prix2, alternative="greater",var.equal=FALSE)
#On peut dire que la concurrence est 


