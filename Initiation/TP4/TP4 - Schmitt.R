#TP4
#Exercice 1
eucO<-read.table("euc0.txt")
head(eucO)
str(eucO)
euc1<-read.table("euc1.txt",header = T)
head(euc1)
str(euc1)
euc2<-read.table("euc2.txt",header = T,sep = ";")
head(euc2)
str(euc2)


#Exercice 2
#1
str(euc1)
summary(euc1)
class(euc1)
head(euc1)
names(euc1)
dim(euc1)
str(euc1)
hauteur
euc1$hauteur
euc1$circ
attach(euc1)
hauteur

#Il y est résumé 4 variables, 2 sont chiffrées par des calcules, les autres "bloc"
#et "clone" sont aussi chiffrés mais sont sans doute des codages

#2
plot(circ,hauteur,mean="Taille des eucalyptus",ylab = "Hauteur",xlab = "Circonférence",pch=3)
lm(hauteur~circ)->lin
abline(lin,col="red")

plot(jitter(circ),jitter(hauteur),mean="Taille des eucalyptus",ylab = "Hauteur",xlab = "Circonférence",pch=3)
lm(hauteur~circ)->lin
abline(lin,col="red")
#Jitter permet de superposé tout les points, les points sont donc tous représentés

plot(hauteur~bloc)
#Elle n'est pas représentative des données

#5
#Ces deux variables sont des variables quantitatives.

mode(clone)
class(clone)
mode(bloc)
class(bloc)

#Dans R leur mode est numérique, mais class nous montre que ce sont des facteurs, c'est que R prend les valeur en codage

#6
plot(hauteur~clone)
#a
boxplot(hauteur,clone)
boxplot(hauteur,clone,range=0, boxwex=0.5) #Recentre les valeurs des boites à moustaches
bloc<-as.factor(bloc)
levels(bloc)<-c("sol_A","sol_B","sol_C")
plot(circ~bloc,range=0, boxwex=0.5,ylab = "Circonférence",xlab="Type de sol")

#7
table(bloc)
table(clone)
par(mfrow=c(1,2)) #représenter un nombre choisi de graph dans la même fenetre
pie(table(bloc))
pie(table(clone))

#8
levels(clone) 
levels(clone)<-c("autres","DH","autres","autres","autres")
str(clone)

filtre<-clone=="autres" #Met un filtre pour les graph
plot(hauteur[filtre]~bloc[filtre],yalb="Hauteur",xlab="Types de sol",range=1, boxwex=0.15,col="blue",main="Eucalyptus qui ne sont pas DH")
#"range" tire les valeurs de la boite à moustache
#"boxwex" permet de changer la taille des boite à moustache

#9
euc2$bloc<-bloc; euc2$clone<-clone
head(euc2)

#10
write.table(euc2,"eucfinal.txt",sep=";",row.names = F)
write.table
