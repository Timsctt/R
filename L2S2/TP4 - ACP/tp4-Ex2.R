#Exercice 2

data<-read.table("BudgetTemps.txt",header= T ,sep = ",")

library("FactoMineR", lib.loc="/Library/Frameworks/R.framework/Versions/3.2
        /Resources/library")

resacp<-PCA(data,quali.sup = c(11,12,13,14))
barplot(resacp$eig[,1], main = "Inertie absolue",
        xlab = "Valeurs propres ou Inertie absolues")
cumsum(Vec2)[4]
#Les 4 premieres variable cumulent 75.95273%

ContriInd1<-resacp$ind$contrib[,1]
Vec<-sort(ContriInd1,decreasing=T)
barplot(Vec,main="Contributions relatives des individus sur l'axe F1",
        xlab="Individus",ylab="Proportion en %",cex.names = 0.7,las=2)
# Les individus qui contribuent a l'axe F1 sont FNW, FNU, FNE, FNY, FMW, FMU et HCE.

vec<-sort(ContriInd1, decreasing = T)

barplot(sort(ContriInd1,decreasing =T), main="Contributions relatives des individus 
        à l'axe F1", ylab="Proportion %", xlab="Individus")

#Pour l'axe F2
ContriInd2<-resacp$ind$contrib[,2]
vec3<-sort(ContriInd2, decreasing = T)
barplot(vec3,main="Contributions relatives des individus 
        à l'axe F2",xlab="",ylab="Proportion en %",ylim=c(0,15),las=2,cex.lab=0.9)
# Les individus qui contribuent a l'axe F2 sont  FCU, HCW, HMW, HAW, FMU, FAU, FMW, 
#HCU, FCW, FNW et FAW.


ContriVar1<-resacp$var$contrib[,1]
Vec2<-sort(ContriVar1,decreasing = T)
barplot(Vec2,main = "Contributions relative des variables à l'axe F1",xlab = "",
        ylab = "Proportion en %",ylim = c(0,25),las=2)
# Les variables qui contribuent a l'axe F1 sont TRAN, PROF, MENA et ENFA. 

ContriVar2<-resacp$var$contrib[,2]
Vec4<-sort(ContriVar2,decreasing = T)
barplot(Vec4,main="Contributions relatives des individus sur l'axe F4",
        xlab="Individus",ylab="Proportion en %",cex.names = 0.7,las=2)
#Les variables qui contribuent a l'axe F2 sont TOIL, COUR, REPA et SOMM.

plot.PCA(resacp,choix = "var",title="Nuages des variables sur el plan F1F2",cex=0.7)
#On peut voir que 4 variables contribuent à l'axe F1 grace aussi au nuages des
#variables

plot.PCA(resacp,choix="ind",title = "Nuages des Individus sur le plan F1F2")

#Caracteristiques des individus
tabInd<-data.frame(round(resacp$ind$coord[,c(1,2,3,4)],2),
                   round(resacp$ind$contrib[,c(1,2,3,4)],2),
                   round(resacp$ind$cos2[,c(1,2,3,4)],4),
                   round(resacp$ind$cos2[,1]+resacp$ind$cos2[,2],4),
                   round(resacp$ind$cos2[,3]+resacp$ind$cos2[,2],4),
                   round(resacp$ind$cos2[,4]+resacp$ind$cos2[,3],4))
colnames(tabInd)<-c("F1","F2","F3","F4","ctr(F1)","ctr(F2)","ctr(F3)","ctr(F4)",
                    "qlt(F1)","qlt(F2)","qlt(F3)","qlt(F4)",
                    "qlt(F1,F2)","qlt(F2,F3)","qlt(F3,F4)")
head(tabInd)