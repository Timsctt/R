
###########
#Exercice 3
###########

#1
df<-iris
attach(df)

#2
n<-sum(Species=="versicolor")
Xbar<-mean(Sepal.Length[Species=="versicolor"])
s<-sd(Sepal.Length[Species=="versicolor"])

a<-0.08
ibinf<-Xbar-qnorm(1-a/2)*s/sqrt(n)
ibsup<-Xbar+qnorm(1-a/2)*s/sqrt(n)
#L'intervale de confiance de l'esperance est donc [5.8082;6.0638]
# au niveau de confiance 92%.

#3
plot(Species,Sepal.Length, main="Repartition de la longueur de sepales
     en fonction de l'espece")

#4
var.test(Sepal.Length[Species=="versicolor"],
         Sepal.Length[Species=="virginica"])

#On peut dire que la variance des longueurs de sepales des iris 
#versicolor et celle des iris virginica est environ la meme, puisque le
#ratio des variances est de 0.6589276, qui est comprit dans 
#l'intervale de confiance a 95%.

#5
t.test(Sepal.Length[Species=="versicolor"],
       Sepal.Length[Species=="virginica"])
#On peut grace au test dire que la moyenne de la longueur des sepales des iris
#versicolor est inferieur a celle des iris virginica, en effet elle est de 
#0.652 de moins.

#6
var.test(Sepal.Length[Species=="versicolor"],
         Sepal.Length[Species=="setosa"])

#On peut dire que la variance des longueurs de sepales des iris 
#versicolor et celle des iris setosa est environ la meme, puisque le
#ratio des variances est de 2.144345, qui est comprit dans 
#l'intervale de confiance a 95%.

t.test(Sepal.Length[Species=="versicolor"],
       Sepal.Length[Species=="setosa"])
#On peut dire, grace au test que la moyenne de la longueur des sepales des iris
#versicolor est superieur a celle des iris setosa, en effet elle est de 
#0.930 de plus.
