#How did Ford (and other car manufacturers in general) segment the overall car market? What was the typical small car marketing strategy in the past? Is the existing segmentation approach still applicable for Ford Ka?

  #Can different demographic variables separate out “Ka Choosers” and “Ka Non-Choosers”?
  #Are there distinct attitudinal segments? If so, how are these segment different and how well can they separate between “Ka Choosers” and “Ka Non-Choosers”?
  #What segmentation approach do you recommend and who is your target buyer? Why?
  #What potential implementation problems do you expect with your recommended approach?
  #Your responses should not exceed two pages (single-space; excluding tables summarizing your statistical results) and need to be uploaded on Canvas (pdf or word format).

DemoData <- read.csv("/Users/guan-hunglai/Desktop/Demographic Data.csv", header = TRUE)

#Can different demographic variables separate out “Ka Choosers” and “Ka Non-Choosers”?
#Performing Segmentation, so cluster analysis
set.seed(1990)
install.packages("NbClust")
install.packages("gmodels")
library(NbClust)
library(gmodels)


DemoData
colnames(DemoData) <- c("PreferenceGroup", "Gender", "Age", "Married","NumChildren","1stPurchase", "AgeCategory","ChildrenCategory","IncomeCategory")

#Histogram
##Prefer and Gender

library(tidyr)
library(dplyr)
KaChooser <- DemoData %>%
                  filter(DemoData$PreferenceGroup == 1)

KaNonChooser <- DemoData %>%
                  filter(DemoData$PreferenceGroup == 2)


hist(KaChooser$Gender) #Similar, can explain
hist(KaNonChooser$Gender)

hist(KaChooser$Age) #Different, cannot explain Ka vs Ka non chooser
hist(KaNonChooser$Age)

hist(KaChooser$Married) #Similar, can explain
hist(KaNonChooser$Married)

hist(KaChooser$NumChildren)  #Similar, can explain
hist(KaNonChooser$NumChildren)

hist(KaChooser$`1stPurchase`) #Similar, can explain
hist(KaNonChooser$`1stPurchase`)

hist(KaChooser$AgeCategory) #Similar, can explain
hist(KaNonChooser$AgeCategory)

hist(KaChooser$ChildrenCategory) #Similar, can explain
hist(KaNonChooser$ChildrenCategory)

hist(KaChooser$IncomeCategory) #Similar, can explain
hist(KaNonChooser$IncomeCategory)


Segstd <- scale(DemoData[, c("PreferenceGroup", "Gender", "Age", "Married","Children","1stPurchase", "AgeCategory","ChildrenCategory","IncomeCategory")])
distance <- dist(Segstd, method = "euclidean")
as.matrix(distance)[1:5, 1:5]

clusteranalysis <- hclust(distance, method = "ward.D2") ###Ward.D2 is the method to find the minimum distance for cluster analysis/ D2 = version 2
plot(clusteranalysis)
clusteranalysis
cluster_2 <- cutree(clusteranalysis, 2)
table(cluster_2) #Cluster 1 = 142, Cluster 2 = 108

SummaryCoeff <- aggregate(Segstd[, c("PreferenceGroup", "Gender", "Age", "Married","#Children","1stPurchase", "AgeCategory","ChildrenCategory","IncomeCategory")], by = list(cluster_2), FUN = mean)

NbClust(Segstd[, 1:5], min.nc = 3, max.nc = 15, index ="all",method = "kmeans")
NbClust(Segstd[,1:5], min.nc=3,max.nc=15, index="all", method="ward.D2")



###Factor Analysis 
library(corrplot)
library(nFactors)
corrplot(cor(DemoData), order ="hclust")
cor(DemoData)
nScree(DemoData, cor = TRUE)

eigenvalue = eigen(cor(DemoData))
eigenvalue$values


###Psychographic Data Segmentation
PsychoData <- read.csv("/Users/guan-hunglai/Desktop/Psychographic Data.csv", header = TRUE)
PsychoData
Segstd <- scale(PsychoData)


NbClust(Segstd[, 1:41], min.nc = 3, max.nc = 15, index ="all",method = "kmeans")
NbClust(Segstd[, 1:41], min.nc = 3, max.nc = 15, index ="all",method = "ward.D2") ## # of clusters = 4

ChooserPsychoData <- cbind(PsychoData, DemoData[,1])

nScree(ChooserPsychoData[,2:41], cor = TRUE) ### also gives 4 factors 
eigenvalue = eigen(cor(ChooserPsychoData[,2:41]))






###Factor Analysis for Psychographic Data
CorrelationMatrix <- as.data.frame(cor(PsychoData[,2:41]))
CorrelationMatrix
library("corrplot")
corrplot(cor(ChooserPsychoData[,2:41]), order ="hclust")

library(nFactors)
nScree(ChooserPsychoData[,2:41], cor = TRUE) ### also gives 4 factors 
eigenvalue = eigen(cor(ChooserPsychoData[,2:41]))
eigenvalue$values
plot(eigenvalue$values, main ="Scree Plot", type = "l", xlab = "cluster numbers")

library(psych)
fit <-principal(ChooserPsychoData[,2:41], nfactors = 3, rotate = "varimax")
fit
fit$values
fit$loadings ##correlation between factors (verbal and math) AND variables (scores)
fit$weights ##adjusting weighted coefficients (kind of like regression coefficients)
fit$scores
colnames(fit$weights) = c("Appearance","Performance","Concept")
colnames(fit$scores) = c("Appearance","Performance","Concept")
reduced_data <- cbind(ChooserPsychoData[,2:41], fit$scores)
head(reduced_data[,1:40])


Choosermean <- aggregate(reduced_data[,c(41:43)], by = list(DemoData[, 1]), FUN = mean, na.rm = TRUE)
Choosermean

library(scatterplot3d)
Scatter3d <- scatterplot3d(Choosermean$Appearance, Choosermean$Performance, Choosermean$Concept,
                           scale.y = 1, type = 'h', asp = 1,
                           xlab = "Appearance", ylab = "Performance", zlab = "Concept",
                           main = "3D Perceptual Mapping")

Scatter3d



tmp<-Choosermean[which(Choosermean$Group.1==1),]
text(Scatter3d$xyz.convert(tmp$Appearance, tmp$Performance, tmp$Concept),
     labels = ("Chooser"),
     col = "red")

tmp2<-Choosermean[which(Choosermean$Group.1==2),]
text(Scatter3d$xyz.convert(tmp2$Appearance,tmp2$Performance,tmp2$Concept),labels=("Non-Chooser"),col="blue")

tmp3<-Choosermean[which(Choosermean$Group.1==3),]
text(Scatter3d$xyz.convert(tmp3$Appearance,tmp3$Performance,tmp3$Concept),labels=("Middle"),col="green")





legend(-4,5.5,legend=c("Chooser","Non-Chooser","Middle"),col=c("red","blue","green"),lty=1)

text(s3d1$xyz.convert(tmp$Recognition,tmp$Practicality,tmp$Performance),labels=("Chooser"),col="red")
tmp2<-reduced_data.mean[which(reduced_data.mean$Group.1==2),]
text(s3d1$xyz.convert(tmp2$Recognition,tmp2$Practicality,tmp2$Performance),labels=("Non-Chooser"),col="blue")
tmp3<-reduced_data.mean[which(reduced_data.mean$Group.1==3),]
text(s3d1$xyz.convert(tmp3$Recognition,tmp3$Practicality,tmp3$Performance),labels=("Middle"),col="green")