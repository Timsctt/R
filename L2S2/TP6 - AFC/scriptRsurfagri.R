
setwd("~/CloudJS/L2ECO-R2-15-16/TP6-AFC")

#Q1
data <- read.table("surfagrmp.txt", header=TRUE, quote="\"")
#Q2
View(data)
chisq.test(data)
# La p-value est inférieure à 0.05 très nettement. On rejette l'hypothèse d'indépendance.

#Q3
test1<-chisq.test(data)
ContriChi2<-(test1$residuals)^2
ContriChi2

#Q4

MargeL<-colSums(ContriChi2)
MargeC<-rowSums(ContriChi2)
ContriChi2M<-rbind(ContriChi2,MargeL)
ContriChi2M<-cbind(ContriChi2,rowSums(ContriChi2))
colnames(ContriChi2M)[ncol(ContriChi2M)]<-"Total"
rownames(ContriChi2M)[nrow(ContriChi2M)]<-"Total"

#ou
ContriChi2MB<-addmargins(ContriChi2)
View(ContriChi2MB)
colnames(ContriChi2MB)[ncol(ContriChi2MB)]<-"Total"
rownames(ContriChi2MB)[nrow(ContriChi2MB)]<-"Total"
View(ContriChi2MB)

#Q5
#Les départements correspondent aux lignes, trions les
sort(MargeC,decreasing=T)
#En divisant par le chi2obs on obtient les ctr relatives :
round(sort(MargeC,decreasing=T)/test1$statistic,2)

#On effectue de même pour les surfaces
sort(MargeL,decreasing=T)
round(sort(MargeL,decreasing=T)/test1$statistic,2)
#On observe que ce sont les surfaces extrêmes qui contribuent le plus.

#Q6
#On va rajouter les marges dans le tableau
?addmargins
dataM<-addmargins(as.matrix(data))
colnames(dataM)[ncol(dataM)]<-"Totaux"
rownames(dataM)[nrow(dataM)]<-"Totaux"
View(dataM)
dataf<-dataM/dataM[nrow(dataM),ncol(dataM)]
#Déterminons un tableau exportable.
datafarr<-round(dataf,3)
datafarr
#Q7 le script suivant permet d'obtenir en même temps la loi marginale est vérifié le total de la ligne à 1.
tab<-addmargins(as.matrix(data),1)  #ajoute le total des colonnes (donc la marge ligne)
dataPL<-prop.table(tab,1) #détermines les profils lignes en divisant par le total ligne.
dataPL<-addmargins(as.matrix(dataPL),2) #ajoute la colonne totale et permet de vérifier la procédure.
View(dataPL)

tab<-addmargins(as.matrix(data),2) 
dataPC<-prop.table(tab,2)
dataPC<-addmargins(as.matrix(dataPC),1)
View(dataPC)

#Q8
#library("FactoMineR", lib.loc="C:/Program Files/R/R-3.1.2/library") #commande à adapter

#Q9
resca<-CA(data)
resca

#Q10
# valeurs propres, résultats pour les colonnes (coord qualité contributions) pour les lignes (id) et les marges.

#Q11
summary.CA(resca)

#Q12
#Graphique inerties relatives
n<-NROW(resca$eig[,1])
coord<-barplot(resca$eig[,2],names.arg=1:n,main="Inerties relatives de chaque axe",width=0.25,space=0.75,xlab="Valeurs propres - Axes factoriels",ylab="Part en %")
segments(coord[-n,],resca$eig[-n,2],coord[-1,],resca$eig[-1,2],lwd=3)

#Graphique inerties relatives cumulées
coord<-barplot(resca$eig[,3],ylim=c(0,105),yaxs="i",ylab="Inerties relatives cumulées",xlab="Axes factoriels - Valeurs propres",main="Inerties relatives cumulées")
segments(coord[-n,],resca$eig[-n,3],coord[-1,],resca$eig[-1,3],lwd=3)
text(coord,resca$eig[,3],round(resca$eig[,3],1),pos=3)

#Q13
sum(resca$eig[,1])
test1$statistic/sum(as.matrix(data))
sum(resca$eig[,1])-test1$statistic/sum(as.matrix(data))

#Q14
#Lignes
attach(resca)
tabL1<-round(row$coord[,1:2],2)
tabL2<-round(row$contrib[,1:2],2)
tabL3<-round(row$cos2[,1:2],2)
P12<-round(row$cos2[,1]+row$cos2[,2],3)
TabCarRow<-data.frame(tabL1,tabL2,tabL3,P12)
colnames(TabCarRow)<-c("F1","F2","ctr(F1)","ctr(F2)","qlt(F1)","qlt(F2)","qlt(F1,F2)")
head(TabCarRow)

#Colonnes
tabC1<-round(col$coord[,1:2],2)
tabC2<-round(col$contrib[,1:2],2)
tabC3<-round(col$cos2[,1:2],2)
P12C<-round(col$cos2[,1]+col$cos2[,2],3)
TabCarCol<-data.frame(tabC1,tabC2,tabC3,P12C)
colnames(TabCarCol)<-c("F1","F2","ctr(F1)","ctr(F2)","qlt(F1)","qlt(F2)","qlt(F1,F2)")
head(TabCarCol)

plot.CA(resca,title="Représentation simultanée Département / Surface Agricole",col.col="black",col.row="red",cex=0.8)

