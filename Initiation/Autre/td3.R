#Exercice 1
#1
M<-matrix(c(1,1,2,0,0,2,2,0,0,0,2,2),ncol = 3,)

#2
colSums(M)
# Fait la somme des colonnes

t(M)
# Transpose la matrice

M*M
#Multiplication terme a terme

M*t(M)
# Ne peut pas se faire car la taille ne correspond pas

M%*%t(M)
# Multiplie les matrices entre elles, on entour * par % pour avoir une multiplication
# matriciel

M%*%M
# Impossible, mauvaise dimension 

M/M
#

M+2*M
#

M-M
# Soustraie la matrice

#3
data<-data.frame(M)
colnames(data)<-c("V1","V2","V3")
rownames(data)<-c("A","B","C","D")

#4
MoyCol<-colMeans(data)
VarCol<-colMeans(data^2)-colMeans(data)^2
etCol<-sqrt(VarCol)
#sd fait l'ecart type corrige = standart deviasion

#5
n<-nrow(data)
n

datac<-data-(matrix(rep(MoyCol,4),byrow = T, nrow = n))
datac
#Matrice qui a etait centree

datacr<-datac/((c(1,1,1,1))%*%t(etCol))
datacr
#Matrice centre reduit, reduit quand on a soustrait l'ecart type. 

datacr2<-scale(data,center=T,scale=T)
datacr2

datacr3<-scale(data,center = T,scale=etCol)
datacr3

#6
cov(data)

cor(data)

cov(datacr)

3/4*cov(datacr) 
#vrai matrice des covariance, matrice des corelation,
#3/4 car (n - 1) / n 

#Elles ne sont pas pareille car c'est aussi des valeurs corrige.  

#7
Mcor<-3/4*cov(datacr)
Mcor

eigen(Mcor)

#9
P<-eigen(Mcor)$vectors
P
#P contient  la matrice de changement de base 

det(P)
#determinent de la matrice P

solve(P)
#La matrice de la matrice P

solve(P) %*%P
#La matrice est la matrice identite, elle a juste une valeur tres proche de 0 calcule 
#avec la puissance 16

t(P)

t(P)%*%(P)

#10
vp<-eigen(Mcor)$values
vp
#calul de valeur propre

IneRel<-vp/sum(vp)
#On divise par la somme de valeur propre, en ACP cela permet de calculer l'inertie

barplot(IneRel)

IneRelCum<-cumsum(IneRel)
IneRelCum

plot(IneRelCum,type="b",ylim = c(0,1))

[...]

#Exercice 2
resACP<-PCA(data)
resACP
resACP$eig
#eig renvois un tableau, contenant les parts d'inertie, et les parts d'inertie cumulee
#du coup on ne prendra seulement la premiere colonne
round(resACP$ind$coord[,c(1,2)],digits = 3)
#Donne les coordonnee des individus, mais seulement sur la premiere et la deuxieme 
#dimension, arrondi trois chiffre apres la virgule
plot.PCA(resACP,choix = "ind", title = "Nuage des individus sur le plan (F1,F2)")
# resultat de l'ACP donne la representation graphique 
