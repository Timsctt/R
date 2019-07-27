#Exercice 1

#1
data<-read.table("DonneesVoitures.txt",sep=";")

#3
resacp<-PCA(data,quali.sup=c(1,6,13))

#4
barplot(resacp$eig[,1],main="VALEURS PROPRES",names.arg=1:nrow(resacp$eig))

#5
summary.PCA(resacp)
#Cette commande donne le resume de l'ACP avec les contributions,
#la qualite de representation et l'inertie...

#6
#a)
ContriInd1<-resacp$ind$contrib[,1]
#b)
names(ContriInd1)<-1:20
#c)
ContriInd2<-sort(ContriInd1,decreasing=T)
#d)
barplot(ContriInd2,main="Contribution relative des individus ? l'axe F1",
        ylab="proportion en %",xlab="Individus")

#e
ContriVoitures<-sort(resacp$ind$contrib[,1],decreasing=T)
ContriVoitures[1:3]
sum(ContriVoitures[1:3])
#Les 3 premieres voitures representent 50% de contribution a l'axe F1.

#f
ContriIndf2<-resacp$ind$contrib[,2]
names(ContriIndf2)<-1:20
ContriIndf3<-sort(ContriIndf2,decreasing=T)
barplot(ContriIndf3,main="Contribution relative des individus ? l'axe F2",
        ylab="proportion en %",xlab="Individus")
ContriVoituresf2<-sort(resacp$ind$contrib[,2],decreasing=T)
ContriVoituresf2[1:4]
sum(ContriVoituresf2[1:4])
#Les 4 premieres voitures contribuent pour 70% de l'axe F2


#7
ContriVar1<-resacp$var$contrib[,1]
names(ContriVar1)<-1:17
ContriVar1<-sort(ContriVar1,decreasing=T)
ContriVar1<-sort(resacp$var$contrib[,1],decreasing=T)
ContriVar1[1:4]

#8
tabInd<-data.frame(round(resacp$ind$coord[,c(1,2,3)],2),
                   round(resacp$ind$contrib[,c(1,2,3)],2),
                   round(resacp$ind$cos2[,c(1,2,3)],3),
                   round(resacp$ind$cos2[,1]+resacp$ind$cos2[,2],3),
                   round(resacp$ind$cos2[,3]+resacp$ind$cos2[,2],3))
               
tabInd

#9
plot.PCA(resacp,choix="ind")
plot.PCA(resacp,choix="ind",title="Nuages des Individus sur le plan F1-F2",cex=0.8)
plot.PCA(resacp,choix="ind",title="Nuages des Individus sur le plan F1,F2",
         selec="cos2 0.6",unselect="grey50",invisible="quali",habillage=6)

#10
plot.PCA(resacp,axes = c(2, 3),choix="ind")

#11

