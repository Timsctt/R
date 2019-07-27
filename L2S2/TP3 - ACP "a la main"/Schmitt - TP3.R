#Exercice 3

M2<-matrix(c(5,2,1,0,2,5,0,-1,1,0,5,-2,0,-1,-2,5),ncol = 4,nrow = 4)
M2

Data<-data.frame(M2)
N<-nrow(Data)

MoyCol<-colMeans(Data)
VarCol<-colMeans(Data^2)-colMeans(Data)^2
etCol<-sqrt(VarCol)

Datac<-Data-(matrix(rep(MoyCol,4),byrow = T, nrow = N))
#matrice centree

Datacr<-Datac/((c(1,1,1,1))%*%t(etCol))
Datacr
#matrice centree reduite

Mcor<-3/4*cov(Datacr)
Mcor

eigen(Mcor)
vp<-eigen(Mcor)$values
#Les valeurs propres sont donc donnee par le vecteurs vp

P<-eigen(Mcor)$vectors

det(P)

P1<-solve(P)
#On note les valeurs avec une tres forte puissance (-16 et -17) qui sont donc tres
#proche de 0
d<-diag(eigen(M2)$values)

P%*%d%*%P1
# Quelques valeurs ne sont pas exacts du fait de la simplification des chiffres après 
# la virgule


# Resolution du système
Sol<-matrix(c(5,-6,5,12),nrow = 4,ncol=1)
M2%*%Sol


#Exercice 4
cidre<-read.table("cidre.txt",header= T ,sep = ";")
setwd("/Users/schmitt/Documents/Cours/L2/Semestre 2 /Logiciel R/TP3")

PCA(cidre)
PCA(cidre)$var$contrib
PCA(cidre)$ind$coord
PCA(cidre)$eig