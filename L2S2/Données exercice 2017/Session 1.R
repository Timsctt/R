#Epreuve sur poste. Session 1 R 2.

###########
#Exercice 1
###########

#1
data<-read.table("JeuDonneesEaux.csv",sep = ";",header = T)
rownames(data)<-data[,1]
data<-data[,-1]
attach(data)

#2
str(data)
mode(data)

#3
data1<-data[data$Gaz == "G","Bicarbonates" ]
data2<-data[data$Gaz == "NG","Bicarbonates" ]
shapiro.test(data1)
shapiro.test(data2)

#4
t.test(data1,mu=1300, power = 0.6, level = 0.1)
t.test(data2,mu=1300, power = 0.6, level = 0.1)

#5
var.test(data1,data2)
#p-value<0.05, on conserve l'hypothese H0 de l'egalite des variance.

t.test(data1,data2)
#la p-value est inférieure 0.1, donc au risque 10% la moyenne de concentration d'ions 
#bicarbonate dans l'eau gazeuse est inferieure a celle dans l'eau non gazeuse.

#6
t.test(Sulfates,Sodium)
#p-value est supérieur a 0.1, donc la moyenne de concentration de sulfates est supérieur
#a celle de Sodium.

#7
median(Sodium)
fort<-data[,1]>median(Sodium)
data[,1]>median(Sodium)
