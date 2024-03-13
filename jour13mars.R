don =read.table("DONNEES/donclassif.txt",header=T,sep=";")
plot(don)
gp3 <- kmeans(don,centers=3)
names(gp3)
gp3$totss
gp3$withinss
gp3$tot.withinss
gp3$betweenss
plot(don,col=gp3$cluster)
##########################################
# choix du nombre de groupes
##########################################
INTER <- 1:30
INTRA <- INTER
for(ii in 1:30){
  tmp <- kmeans(don,centers=ii)
  INTER[ii] <- tmp$betweens
  INTRA[ii] <- tmp$tot.withinss
}
INTER <- INTER/tmp$totss*100
INTRA <- INTRA/tmp$totss*100
plot(1:30,INTER,type="h")
points(1:30,INTRA)
##################################
gp4 <- kmeans(don,centers=4,nstart=100)
plot(don,col=gp4$cluster,pch=16,cex=0.5)
gp4$betweenss
points(gp4$centers,pch=16)

doncr <- scale(don)
for(ii in 1:30){
  tmp <- kmeans(doncr,centers=ii,nstart=10)
  INTER[ii] <- tmp$betweens
  INTRA[ii] <- tmp$tot.withinss
}
INTER <- INTER/tmp$totss*100
INTRA <- INTRA/tmp$totss*100
plot(1:30,INTER,type="h")
points(1:30,INTRA)
#
gp4 <- kmeans(doncr,centers=4,nstart=100)
plot(doncr,col=gp4$cluster,pch=16,cex=0.5)
gp4$betweenss
#########################################
#########################################
donM <- dist(don)
clhs <- hclust(donM,method = "single")
plot(clhs)
plot(sort(clhs$height,dec=T)[1:20],type="h")
abline(h=0.6)
gps <- cutree(clhs,h=0.6)
plot(don,col=gps)
###########################
table(gps)


clhc <- hclust(donM,method = "complete")
plot(clhc)
plot(sort(clhc$height,dec=T)[1:20],type="h")
gpc <- cutree(clhc,k=4)
plot(don,col=gpc)
table(gpc)


clha <- hclust(donM,method = "average")
plot(clha)
plot(sort(clha$height,dec=T)[1:20],type="h")
gpa <- cutree(clha,k=4)
plot(don,col=gpa)
table(gpa)

clhw <- hclust(donM,method = "ward.D2")
plot(clhw)
plot(sort(clhw$height,dec=T)[1:20],type="h")
gpw <- cutree(clhw,k=4)
plot(don,col=gpw)
table(gpw)




donM <- dist(don,method="manhattan")
clhs <- hclust(donM,method = "single")
plot(sort(clhs$height,dec=T)[1:20],type="h")
abline(h=0.7)
gps <- cutree(clhs,h=0.7)
plot(don,col=gps)
table(gps)


clhc <- hclust(donM,method = "complete")
plot(sort(clhc$height,dec=T)[1:20],type="h")
gpc <- cutree(clhc,k=4)
plot(don,col=gpc)
table(gpc)


clha <- hclust(donM,method = "average")
plot(sort(clha$height,dec=T)[1:20],type="h")
gpa <- cutree(clha,k=4)
plot(don,col=gpa)
table(gpa)

clhw <- hclust(donM,method = "ward.D2")
plot(sort(clhw$height,dec=T)[1:20],type="h")
gpw <- cutree(clhw,k=4)
plot(don,col=gpw)
table(gpw)



don =read.table("DONNEES/donclassif2.txt",header=T,sep=";")
donM <- dist(don)
clhs <- hclust(donM,method = "single")
### mince !

tmp <- kmeans(don,centers=10000)
dontmp <- tmp$centers
range(tmp$size)
plot(dontmp)
donM <- dist(dontmp)
clha <- hclust(donM,method = "average")
plot(sort(clha$height,dec=T)[1:20],type="h")
gpa <- cutree(clha,k=5)
clind <- gpa[tmp$cluster]
plot(don,col=clind)



don =read.table("DONNEES/donclassif.txt",header=T,sep=";")
library(fpc)
library(dbscan)
titi <- fpc::dbscan(don,eps=0.25)
toto <- dbscan::dbscan(don,eps=0.25)
table(titi$cluster,toto$cluster)
library(microbenchmark)
temps <- microbenchmark(fpc::dbscan(don,eps=0.25),
                        dbscan::dbscan(don,eps=0.25),times=10)
plot(temps)
temps
###donc on va utiliser dbscan
###choix d'eps 
kNNdistplot(don,3)
abline(h=0.21)
toto <- dbscan::dbscan(don,eps=0.21)
plot(don,col=toto$cluster)
plot(don[toto$cluster==0,],col=1)
table(toto$cluster)
toto <- dbscan::dbscan(don,eps=0.35)


library(mclust)
classif <- Mclust(don,1:20)
summary(classif)
plot(classif)

classif4 <- Mclust(don,4)
plot(classif4)
classif4 <- Mclust(don,4,"VVE")
plot(classif4)



####on reprend le data camp
don=readRDS("donh.RDS")
donTT <- don[1:1000,]
donA <- don[-c(1:1000),]
##########################################
##gp sur donA sans y
library(tidyverse)
don <- select(donA,-Y)
############################################
INTER <- 1:30
INTRA <- INTER
for(ii in 1:30){
  tmp <- kmeans(don,centers=ii,nstart=3)
  INTER[ii] <- tmp$betweens
  INTRA[ii] <- tmp$tot.withinss
}
INTER <- INTER/tmp$totss*100
INTRA <- INTRA/tmp$totss*100
plot(1:30,INTER,type="h")
gp3 <-kmeans(don,3,nstart=10)
boxplot(donA$Y~gp3$cluster)
### genial !
library(mclust)
class <- Mclust(don,1:4,"EII")
plot(class$BIC)
################################
distaucentre <- function(x,centre){
  k=nrow(centre)
  choix <- 1:k
  for(ii in 1:k){
    choix[ii] <- sum((x-centre[ii,])^2)
  }
  which.min(choix)
}

aff <- 1:nrow(donTT)
CENTRE <- gp3$centers
for(ii in 1:nrow(donTT)){
  aff[ii] <- distaucentre(donTT[ii,-1],CENTRE)
}
plot(aff)


library(FactoMineR)
library(Factoshiny)
data("decathlon")
deca=decathlon[,1:10]
Factoshiny(deca)

resPCA <- PCA(deca)
names(resPCA)
resPCA$var$contrib


don =read.table("DONNEES/temperat.txt",header=T,sep=";",dec=",")
don <- don[,1:12]
matplot(t(don),type="l")
