#Exercice 4
data<-read.table("filmsencore.txt",header= T ,sep = ";")

#1
attach(data)
rep<-c(0.05,0.25,0.08,0.49,0.13)
levels(data$genre)
names(rep)<-c("Action","Comedie","Documentaire","Drame","Romance")
Action<-genre[genre=="Action"]
as.numeric(data$genre)
hyper<-c(sum(genre=="Action"&year>=2000),sum(genre=="Comedie"&year>=2000),
         sum(genre=="Documentaire"&year>=2000),
         sum(genre=="Drame"&year>=2000),
         sum(genre=="Romance"&year>=2000))
chisq.test(hyper,p=rep)
#La p-value est petite,proche de 0, donc on conclut que l'etude ne met pas en avant
#une difference sur la repartition des genres pour les films produits a partir
#des annees 2000.

barplot(hyper,main="Effectifs de l'echantillon",col="orange",cex.names=0.95,space=1)
barplot(rep,main="Repartition de l'echantillon",col="blue",cex.names=0.95,space=1)

#2
hyper2<-c(sum(genre=="Action"&year<=1950),sum(genre=="Comedie"&year<=1950),
          sum(genre=="Documentaire"&year<=1950),
          sum(genre=="Drame"&year<=1950),
          sum(genre=="Romance"&year<=1950))

test2<-chisq.test(hyper2,p=rep);test2
#On obtient une p-value inférieur à 0.05. Donc on conclut que l'echantillon met
#bien en avant une difference de l'etude des repartitions des genres pour les 
#films jusqu'en 1950.

barplot(hyper2,main="Effectifs de l'echantillon",col="orange",cex.names=0.95,space=1)
barplot(rep,main="Repartition de l'echantillon",col="blue",cex.names=0.95,space=1)

#3
#a
longueur<-rep("moyen",nrow(data))
longueur[length<80]<-"court"
longueur[length>100]<-"long"

#b
table<-table(data.frame(longueur,genre));table
test3<-chisq.test(table);test3

#On obtient une p-value inférieur à 0.05. Donc on conclut qu'il n'y a pas de 
#dependance entre la duree du film et le genre correspondant

#4
prop.test(sum(genre=="Action"&rating<=5),1000,0.05,alternative="less")
#On obtient une p-value inférieur à 0.05. On rejette donc l'hypothese H0.On en
#conclut donc les films d'action sont bien les films les moins bien notes

prop.test(sum(genre=="Documentaire"&rating>=8),1000,0.05,alternative="greater")
#On obtient une p-value inférieur à 0.05. On rejette donc l'hypothese H0. On 
#en conclut donc que les documentaires sont bien les films les mieux notes



#Exercice 5
#1
Voiture<-read.table("DonneesVoitures.txt",sep=";",header=T,row.names=1)
head(Voiture)
attach(Voiture)

t.test(C1,C2)
#La p-value etant proche de 0, on conclut que non la consommation est plus 
#forte en ville que sur route
t.test(C1,C3)
#Meme chose, la p-value inferieur a 0.5, on en conclut que les voiture consomme 
#plus en ville que sur autoroute

#2
