#TP5


##############
#Exercice 1
##############

#1
data<-read.table("Donnees.txt",sep=";",header=T)

#2
nrow(data)

#3
str(data)

#4
attach(data)
summary(data)
levels(Diplome)

#5
hist(DureeTV,main = "Etude du temps passé devant la TV",
     xlab = "Duree quotidienne devant la TV", ylab="Effectifs", col = "orange")

#6
sum(DureeTV>=120)

#7
dataF<-data[data$Genre=="F",]

#8
dataF<-dataF[,-1]
head(dataF)

#9
boxplot(DureeTV~Foyers,boxwex=0.5,xlab="Nombre de personnes dans le foyer",
        ylab="Duree devant la TV",main="Duree devant la TV",range=0)

#10
round(mean(DureeTV[Foyers==4]),1)

#11
Vec<-rep(1,nrow(data))
Vec[Diplome=="Pas de diplome"]<-0

#12
sum(Genre=="H"[Vec=1])/nrow(Vec)
round(sum(Genre=="H"[Vec=1])/sum(Vec),3)

#13
table(Genre,Diplome)

#14
cor(Foyers[Genre=="H"],DureeTV[Genre=="H"])
cor(Foyers[Genre=="F"],DureeTV[Genre=="F"])
x<-Foyers[Genre=="F"]
y<-DureeTV[Genre=="F"]
plot(x,y,xlab="Nb Personnes",ylab="Duree TV",main="Etude pour les femmes")
lm(y~x)->reg
abline(reg,col="red",lwd=2)