#TP5 Test du X2 et autres
#Exercice 1
data<-read.table("sommeil.txt",header= T ,dec=",", row.names=1, sep = " ")
#on marque la decimale avce le "dec=",""

#1
#Les deux echantillons ne sont pas independants, ils sont appariees, puisque le 
#medicament est donne pour les deux echantillons 

#2
t.test(data$M1,data$M2,paired=T, alternative = "less")
#On cherche H1: M1<M2, donc on met "less"

#3
#Le medicament M2 est plus efficace que le medicament M1, la p-value est tres petite,
#proche de 0.

#Exercice 2
#1

tab1<-matrix(c(592,544,119,97,849,677,504,451,36,14),ncol=5)
rownames(tab1)<-c("Garcon","Fille")
colnames(tab1)<-c("Blond","Roux","Chatain","Brun","Noir")
tab1

#2
par(mfrow=c(2,1))
couleur<-c("Gold","OrangeRed","Goldenrod","Brown","Black")
barplot(tab1[1,],main="Garcon",col=couleur)
barplot(tab1[2,],main="Filles",col = couleur)

#3
chisq.test(tab)

#4
resultat<-chisq.test(tab1); resultat$expected

sum(resultat$expected<5)

min(resultat$expected)

#5
resultat$residuals

resultat$residuals^2

contri<-resultat$residuals^2; contri

round((contri[1,]+contri[2,])/resultat$statistic*100,0)
#En pourcentage, noir est le plus grand.

#6
tab2<-tab1[-5,]
tab2

chisq.test(tab2)
#On conclue pour ce nouveau test que l'on ne rejette pas, puisque la p-value=0.27


#Exercice 3
#1
hyper<-matrix(c(225,77,99,1,0),ncol=5)
rep<-c(0.51,0.17,0.27,0.04,0.01)
rownames(hyper)<-c("Nombre de patients")
colnames(hyper)<-c("Hindous","Musulmans","Créoles","Chinois","Autres")
hyper

#2
barplot(hyper,main="Hypertension des différentes ethnies",col = "orange",las=2)
barplot(rep,main="Effectif théorique",col = "orange")


#3
test1<-chisq.test(hyper,p=rep); test1

#4
test1$expected

#5
hyper2<-hyper[,-5]; hyper2
rep2<-c(0.51,0.17,0.27,0.05)
rep2<-rep(c(1,2,3),rep[,4]+rep[,5])

#6
chisq.test(hyper2,p=rep2)

#7
Resultat<-chisq.test(hyper2); Resultat$expected

chisq.test(Resultat,p=rep2)

#Exercice 4
#1
film<-read.table("filmsencore.txt",header=T,sep = ";")
film
