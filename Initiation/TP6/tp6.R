#TP6


##############
#Exercice 1
##############
#1
df<-read.table("CollegeDistance.csv",sep = ";", header= T)
head(df)

#2
mean(df$ed)
mean(df$ed[df$female==0])

#3
sd(df$bytest[df$hispanic==1])

#4
sum(df$hispanic==1&df$black==1)
which(df$hispanic==1&df$black==1)

#5
hist(df$bytest, main="Répartition des étudiants en fonction du bytest", xlab= "bytest", 
     ylab = "Effectifs",col="orange")

#6
n<-nrow(df)
commu<-rep("W",n)
commu[df$black==1]<-"B"
commu[df$hispanic==1]<-"H"

#7
summary(df$ed[commu=="B"])
summary(df$ed[commu=="H"])
summary(df$ed[commu=="W"])

#8
par_sup<-rep(1,n)
par_sup[df$dadcoll==0&df$momcoll==0]<-0

df$par_sup<-par_sup

#9
df50<-df[df$bytest>=50,]

#10
colnames(df)
filtre<-which(colnames(df)=="black"|colnames(df)=="hispanic"|colnames(df)=="par_sup")
df50b<-df50[,-filtre]
head(df50b)



#############
#Exercice 2
#############
#Exercice 2
#1
dat<-read.table("Lun_Mar.txt",sep="\t",header=T)
head(dat)

#2
sum(dat$Origine=="M")

#3
Luh<-dat[dat$Sexe==1&dat$Origine=="L",]
Luh

mean(Luh$Taille)

hist(Luh$Observation,Luh$Taille)

hist(Luh$Taille,main="Répartition des luniens hommesen fonction de leur taille",
     ylab="Effectifs", xlab="Tailles")

sum(Luh$Couleur==1)

#4
boxplot(dat$Taille[dat$Couleur==0],horizontal = T,
        main="Taille des extra-terrestres rouge", xlab="Effectif")

#5
sum(dat$Taille>155)/nrow(dat)

#6
Q1<-quantile(dat$Taille,0.25)
Q2<-quantile(dat$Taille,0.75)

#7
qual<-rep("moyen",nrow(dat))
qual[dat$Taille<Q1]<-"petit"
qual[dat$Taille>=Q3]<-"grand"
dat$qual<-qual
head(dat)
View(dat)

