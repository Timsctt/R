###TP6

####
#exercice 1
####
#1
data<-read.table("surfagrmp.txt", sep = " ", header = T)

#2
chisq.test(data)
#La p-value est très proche de 0, donc l'hypothese d'independance hO est rejetee.

#3
test1<-chisq.test(data)
ContriChi2<-(test1$residuals)^2

#4
ContriChi2M<-addmargins(ContriChi2)
colnames(ContriChi2M)[ncol(ContriChi2M)]<-"Total"
rownames(ContriChi2M)[nrow(ContriChi2M)]<-"Total"

#5
(sort(rowSums(ContriChi2),decreasing=T)/chisq.test(data)$statistic)*100
#H.P (32%), AVER (24%), GERS (10%)

(sort(colSums(ContriChi2),decreasing=T)/chisq.test(data)$statistic)*100
#INFO5 (33%), SUP50 (28%), S3550 (13%)

#6
dataMa<-addmargins(as.matrix(data))
dataf<-data/dataMa[nrow(dataMa),ncol(dataMa)]

#7
tab1<-addmargins(as.matrix(data),1)
data.PL<-prop.table(tab1,1)

tab2<-addmargins(as.matrix(data),2)
data.PC<-prop.table(tab2,2)

#8
library("FactoMineR", 
        lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

#9
resca<-CA(data)
resca

#10
#Les valeurs propres, les profils ligne, les profils colonnes

#11
summary.CA(resca)

#12
