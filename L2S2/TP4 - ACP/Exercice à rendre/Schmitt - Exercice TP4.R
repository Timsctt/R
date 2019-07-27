############
#Exercice 2
############


#1
df<-read.table("DonneesAlimCSP.csv",sep = ";",header= T)
rownames(df)<-df[,1]
df<-df[,-1]

#2
nb<-c(2,2,2,3,3,3,4,4,4,5,5,5)

#3
df$nb<-nb

#4
library("FactoMineR", 
        lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
resacp<-PCA(df, quanti.sup = 8)

#5
sum(resacp$eig[,1]>1)

#6
ContriVar<-sort(resacp$var$contrib[,1], decreasing = T)
barplot(ContriVar,main="Part de contribution a l'axe F1 des variables",ylab="",
        xlab="",las=3)

#7
ContriVar[1:4]

#8
qualInd1<-(resacp$ind$cos2[,1]+resacp$ind$cos2[,2])
qualInd1[qualInd1<0.8]

#9
plot.PCA(resacp,choix="ind",title="Nuages des Individus sur le plan F1,F2",
         select="cos2 0.8")


#10
TabCar<-data.frame(round(resacp$var$coord[,c(1,2)],2),
                round(resacp$var$contrib[,c(1,2)],2),
                round(resacp$var$cos2[,c(1,2)],3),
                round(resacp$var$cos2[,1]+resacp$var$cos2[,2],3))
colnames(TabCar)<-c("F1","F2","ctrF1","ctrF2","qltF1","qltF2","qlt(F1,F2)")
TabCar
