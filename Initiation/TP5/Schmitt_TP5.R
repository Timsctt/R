#Exercice 2
#1
data<-read.table("Donnees.txt",header = T,sep = ";")
data

#2
nrow(data)
#C'est un échantillon de 807 observations

#3
str(data)
#Les différentes variables sont donc le genre, homme ou femme, le nombre de personnes 
#dans le foyers, le diplome de la personne observée et enfin le temps passé à regarder
#la télévision

#4
summary(data)
levels(data$Diplome)

#5
hist(data$DureeTV, main="Durée quotidienne devant la télévision",ylab="Effectif",
     xlab = "Temps en minutes devant la TV",cex.lab = 1.5,
     cex.axis = 0.9,col.lab = "red", col.axis = "black",col="blue")

#6
NROW(data$DureeTV[data$DureeTV>=120])

#7
dataF<-data[data$Genre=="F",]
dataF

#8
dataF[,-1]

#9
attach(data)
Foyers<-as.factor(Foyers)
plot(Foyers,DureeTV,main="Durée quotidienne devant la TV en fonction du foyers",
     xlab="Nombre de personnes dans le foyers",ylab="Temps devant la TV (en minutes)",
     range=0,col="orange",boxwex=0.5,border="blue",col.lab="navy blue",cex.lab=0.9,
     cex.axis=0.9)

#10
Foyers<-as.integer(Foyers)
round(mean(DureeTV[Foyers==4]),1)

#11
Vec<-rep("1",nrow(data))
Vec[Diplome=="Pas de diplome"]<-0
Vec[Diplome=="BEPC"]<-0
Vec[Diplome=="CAP,BEP"]<-0
Vec

#12
NROW(Vec[Genre=="H"])   #Nombre d'homme totale dans l'echantillon
NROW(Vec[Vec==1&Genre=="H"])  #Nombre d'homme ayant le bac ou plus dans la totalité de 
#l'echantillon
round((NROW(Vec[Vec==1&Genre=="H"])/NROW(Vec[Genre=="H"]))*100,digits=4)  # Calcule de
#pourcentage des hommes ayant le bac ou plus dans l'echantillon, arrondi au millième

#13
ftable(Genre, Diplome)

#14
data.lm<-lm(Foyers ~ Diplome*Genre, data)
data.lm
summary(data.lm)
#On prend la p-value la plus élevée