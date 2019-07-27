###### question 1
data<-read.csv("JeuDonneesEaux.csv",header=TRUE,sep=";")
head(data)

#####question 2
str(data)
# les var eaux etgaz sont qualitative les autres quantitative

### question 3
shapiro.test(data$Bicarbonates)
# p value faiblerejt h0 donc loi non normale

### question 4
big<-data$Bicarbonates[data$Gaz=="G"]
power.t.test(n=13,sd=sd(big),power = 0.6,alternative = "two.sided",type="one.sample",sig.level = 0.1)
# on trouve un delta de 735.5934

### question 5
bing<-data$Bicarbonates[data$Gaz=="NG"]
bing
var.test(big,bing)
t.test(big,bing,alternative="greater",var.equal = FALSE,paired=FALSE)
## p value faible rejet h0 doncla concentration en bicarbonate est plus grande pour les eaux gazeuse que non gazeuse

### question 6
var.test(data$Sulfates,data$Sodium)
t.test(data$Sulfates,data$Sodium,alternative="greater",var.equal = TRUE,paired=FALSE)
# concentration en sulfates n'est pas significativement plus gra,de sue celle de sodium

### question 7
data2<-data
median(data2$Sodium)
data2sod<-rep("fort",nrow(data2))
data2sod[data2$Sodium<=13.5]<-"faible"
data2$Sodium<-data2sod
data2

### question 8
chisq.test(data2$Sodium,data2$Gaz)
### p value faible rejet ind?pendance





### quetion 1 
library("FactoMineR")


### question 2
pcadata<-PCA(data,quali.sup=c(1,10))

### question 3
barplot(pcadata$eig[,3], main="Inertie relative cumul?e")
pcadata$eig[,1]>1

### on conserve 3 axe

### question 4
inertie<-pcadata$eig[,2]
round(sum(inertie[1:3]),2)

### question 5
contriind3<-pcadata$ind$contri[,3]
contriind3<-sort(contriind3,decreasing = TRUE)
contriind3[1]
data[16,]
# il s'agit de l'eau parot

### question 6
pcadata$var$cos2
round(pcadata$var$cos2[3,2]+pcadata$var$cos2[3,3],3)


### question 7
par(mar=c(3,2,2,1))
plot.PCA(pcadata,choix = "ind",title="Nuages des Individus sur le plan F1,F2",habillage = 10
         ,select="cos2 0.8",unselect="grey50")
