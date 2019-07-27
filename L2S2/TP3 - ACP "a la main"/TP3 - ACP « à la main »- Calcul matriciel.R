#Exercice 1
#1 et 2
data<-read.table("DonneesVoitures.txt",header= T ,sep = ";", row.names=1)

#3
resacp<-PCA(data,quali.sup = c(1,6,13))
#On ne prend que les variables quantitatives, quali.sup correspond à ajouter 
#des variables pour les garder en qualitatives
#Sur le 1er graphique il y en a des rose, qui sont les qualitatives

#4
barplot(resacp$eig [,1], col = "orange", main = "Inerties absolues", cex.names = 0.9,
        cex.axis = 0.9, names=1:nrow(resacp$eig))

barplot(resacp$eig[,3],main = "Inerties absolues cumulees")

resacp$eig[3,3]
#part cumulee des trois premiers axes

resacp$eig[1,3]

resacp$eig[2,3]

#5
summary.PCA(resacp,file="resume.txt")
#Sort un résumé des données, crée un fichier txt qui contient plusieurs infos 

#6
#a)
ContriInd1<-resacp$ind$contrib[,1]

#b)
names(ContriInd1)<-1:20
#On a donnée des noms aux vecteurs, pour avoir les noms en bas

#c)
vec<-sort(ContriInd1, decreasing = T)

#d)
barplot(sort(ContriInd1,decreasing =T), main="Contributions relatives des individus 
        à l'axe F1", ylab="Proportion %", xlab="Individus")

#e)
vec[1:3]

cumsum(vec)[1:5]

names(vec[1:3])

row.names(data)[1]

row.names(data)[c(12,13,3)]

row.names(data)[as.numeric(names(vec[1:3]))]
#On sort le nom des 3 voitures contribuant le plus à l'axe F1

#f)
ContriInd2<-resacp$ind$contrib[,2]
names(ContriInd2)<-1:20
barplot(sort(ContriInd2,decreasing =T), main="Contributions relatives des individus 
        à l'axe F1", ylab="Proportion %", xlab="Individus")
vec2<-sort(ContriInd2, decreasing = T)
vec2[1:4]
row.names(data)[as.numeric(names(vec2[1:4]))]

#7
ContriVar<-resacp$var$contrib[,2]
names(ContriVar)<-1:16
sort(ContriVar,decreasing = T)
cumsum(sort(ContriVar,decreasing = T))
barplot(cumsum(sort(ContriVar,decreasing =T)))
barplot(sort(ContriVar,decreasing = T))

vec3<-sort(ContriVar, decreasing = T)
vec3[1:4]
row.names(data)[as.numeric(names(vec3[1:4]))]

attach(resacp)
#Individus
tab1<-round(ind$coord[,1:3],2)
tab1

plot.PCA(resacp,choix="ind")
plot.PCA(resacp,choix="var")


plot.PCA(resacp,choix="ind", title="",select = "cos2 0.6", unselect="grey50",
         invisible="quali", habillage = 6)
#,select = "cos2 0.6" permet selectionner les qualité de représentation, et ici la
#qualité de représentation est de 0.6. On peut l'augmenter à 0.8
plot.PCA(resacp,choix="ind", title="",select = "cos2 0.8", unselect="grey50",
         invisible="quali", habillage = 6)
#Ici les individus on était selctionner par la qualité des individus de plus de 0.8

# invisible="quali" les variable qualitative qui nous intéresse pas on les fait 
#disparaitre.$

#habillage = 6 nous permet mettre une légende sur la trans avant ou arrière.