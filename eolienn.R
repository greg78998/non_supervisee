library(mclust)
library(dplyr)
library(tidyverse)


setwd("C:/Users/cepe-s4-03/Desktop/greg/CLASSIF/DONNEES")
don_ori <- readRDS("donh.RDS")
don <- don_ori 

donT <- don[1:1000,]

donA <- don[-c(1:1000),]
donA_withoutY <- donA %>% select(-Y)

## on utilise la décomposition par densité


set.seed(1234)

nbC <- 15
part <- data.frame(k= seq(1,nbC))

for (ii in seq(1:nbC)){
  km <- kmeans(donA, centers = ii, nstart = 100)
  part[ii,"btw"]=round(km$betweenss,0.1)
  part[ii,"tot.withing"]=km$tot.withinss
  part[ii,"tot"]=km$totss
  part[ii,"eq. R2"]=sum(km$withinss)/km$totss*100
}
plot(part$k, part$`eq. R2`)

km_best <- kmeans(donA, centers = 3, nstart = 100)

donA$group <-km_best$cluster 
boxplot(Y~group, donA)

table(donA$group)

foo <- mclust::Mclust(donA %>% select(-Y),
                      2:4,"VVV")

don <- donA %>% filter(group == 3)










