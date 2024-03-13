



setwd("C:/Users/cepe-s4-03/Desktop/greg/CLASSIF/DONNEES")

nbC <- 40
table <- read.table("donclassif.txt", sep = ";", header = TRUE)
part <- data.frame(x = seq(1,nbC))

table$V1 <- scale(table$V1)
table$V2 <- scale(table$V2)

plot(table, main = "Données de base")


set.seed(1234)

for (ii in seq(1:nbC)){
  km <- kmeans(table, centers = ii, nstart = 100)
  part[ii,"btw"]=round(km$betweenss,0.1)
  part[ii,"tot.withing"]=km$tot.withinss
  part[ii,"tot"]=km$totss
  part[ii,"eq. R2"]=sum(km$withinss)/km$totss*100
  plot(table, col=km$cluster, main = paste("Nb groupes : ", ii, sep = ""))
  points(km$centers, pch = 16)
}

part


tmp <- kmeans(table, centers= 5000)
dontmp <- tmp$centers
dibL <- dist(dontmp)

cah <- hclust(dist(dibL), method="average")
plot(cah)

plot(sort(cah$height,dec=T),type="h")
plot(sort(cah$height,dec=T)[1:20],type="h")

#gpcah <- cutree(cah,h=0.15)
gpa <- cutree(cah,k=4)

clid <- gpa[tmp$cluster]
plot(don, col = clid)


plot(table, col = gpcah)
table(gpcah)



cah <- hclust(dist(table), method="complete")
plot(cah)

plot(sort(cah$height,dec=T)[1:20],type="h")

gpcah <- cutree(cah,k=4)

plot(table, col = gpcah)
table(gpcah)




cah <- hclust(dist(table), method="average")
plot(cah)

plot(sort(cah$height,dec=T)[1:20],type="h")

gpcah <- cutree(cah,k=4)

plot(table, col = gpcah)
table(gpcah)


cah <- hclust(dist(table), method="ward.D2")
plot(cah)

plot(sort(cah$height,dec=T)[1:20],type="h")

gpcah <- cutree(cah,k=4)

plot(table, col = gpcah)
table(gpcah)



