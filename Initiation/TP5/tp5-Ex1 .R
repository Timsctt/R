#Exercice 1
#1
data<-read.table(file="http://biostatisticien.eu/springeR/imcenfant.txt", sep="",dec=".",header = T)
head(data)
colnames(data) #nom des différentes colonnes
colnames(data)[1]<-"sexe"  #[1] pour la première colonne, puis renomer par : <-"..."
colnames(data)
head(data)
#Change le nom de la colonne choisi "[numero colonne]", cela permet de changer des
#titres de colonne.
str(data)
nrow(data)
#La taille de la population est de 152 observations, la commande nrow permet d'avoir 
#le nombre directement, lenght est pour un vecteur.

#2
attach(data)
mean(poids) #moyenne des poids
mean(poids[an>=4]) #moyenne des poids des enfants d'au moins 4 ans
mean(poids[!(an<4)]) # ! donne la négation 
# mean(poids[!(an<4)]) = mean(poids[an>=4])
quantile(poids[sexe=="F"],0.25) # quantile du premier quart
sd (poids[sexe=="G"&an>3]) #sd écart type corrigé des poids des 
#garçons de plus de 3ans

#3
str(zep)
class(zep)
median(poids[zep=2]) #median = médiane des poids 
quantile(poids[zep=1&sexe=="F"],0.75)

#4
tab1<-data[,c(1,3)] #avec la virgule en premier cela selectionne seulement
#les COLONNES 1 et 3 
head(tab1)
tab2<-data[-(1:3),]
head(tab2) # en mettant la virgule en second membre cela supprime les 
#VALEURS de 1 à 3
tab3<-data[sexe=="F",]
head(tab3) # Selectionne les données concernant seulement les filles
tab4<-tab3[,-1] 
head(tab4) # Cela a juste supprimé la première colonne, ici "le sexe"
tab5<-data[sexe=="F",-1]
head(tab5) # Additionne les deux dernière commandes, c'est à dire : selectionner les 
#filles plus supprimmer la première colonne
tab6<-data[zep=="O"&poids>=16,]
head(tab6) # selection des habitants en zep ayant un poids supérieur ou égale à 16
tab7<-data[sexe=="G",c(-1,-2)]
head(tab7) # selection des garçon plus suppression de la colonne 1 et 2

colnames(data=="zep",colnames(data)=="taille") #
tab7<-data[zep=="o"&sexe=="G",-c(1,2)]

tab7<-data[zep=="o"&sexe=="G",]
tab7<-tab7[,!colnames(tab7)=="sexe"]
tab7<-tab7[,colnames(tab7)=="zep"]
head(tab7)

#6
detach(data)
data$taille<-data$taille/100

#7
IMC<-data$poids/data$taille^2 # Calcule de l'IMC
IMC<-round(data$poids/data$taille^2,1) # Une décimale ",1"
IMC


#8
data$IMC<-IMC # Insère la colonne IMC dans le tableau
data

#9
age<-data$an+data$mois/12 # Calcule de l'age plus le nombre de mois
age<-round((data$an+data$mois/12),1) # arrondi du calcule à une décimale
data
data$age<-age 
data

#10
tab8<-data[data$IMC<15&data$age<3.5,] # enfants ayant un IMC inférieur à 15 et agé de
# plus de 3 ans 6 mois
head(tab8)

#11
qual<-rep("normal",nrow(data)) # nrow RAPPEL : nombre d'observation 
# Ici on affecte la notion normal à tout les enfants
qual[data$IMC>18]<-"obesite"     # On determine la première tranche du cas non-normal
qual[data$IMC<13.5]<-"insuffisance ponderale"   # Deuxième tranche des non-normal
qual
data$qual<-qual
row.names(data)[data$qual=="insuffisance ponderale"]

#12
write.table(data,"TP5_data.txt",sep=";",dec = ".",row.names=F,col.names=T)