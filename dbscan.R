library(dbscan)

setwd("C:/Users/cepe-s4-03/Desktop/greg/CLASSIF/DONNEES")

table <- read.table("donclassif.txt", sep = ";", header = TRUE)
table<- scale(table)
plot(table)


library(microbenchmark)
temps <- microbenchmark(fpc::dbscan(table, eps = 0.25), 
                        dbscan::dbscan(table, eps = 0.25), times = 10)
plot(temps)



#### dbscan est plus rapide du cou, on va utilier dbscan

dbscan::kNNdistplot(table,3)
abline(h = 0.21)

res <- dbscan::dbscan(table, eps = 0.1)
pairs(table, col = res$cluster + 1L)

plot(table, col = res$cluster)
table(res$cluster)


points(table[res$cluster == 0, ], pch = 3, col = "grey")


hullplot(table, res)




library(fpc)


Dbscan_cl <- fpc::dbscan(table, eps = 0.25, MinPts = 5) 
Dbscan_cl 

# Checking cluster 
Dbscan_cl$cluster 


# Plotting Cluster 
plot(Dbscan_cl, table, main = "DBScan") 
plot(Dbscan_cl, table, main = "exercice sur dbscan (packages fpc)") 

?mclust




library(mclust)
foo <- mclust::Mclust(table,4, verbose = interactive())
summary(foo)
names(summary(foo))
plot(foo)

summary(foo)$pro 

summary(foo)$bic
plot(table, col=summary(foo)$classification,cex=.3,main="K=12")

foo$modelName
names(foo)

foo$bic
foo$BIC
foo$modelName # que le meilleur résultat
foo$classification #### la classification associé au meilleur résultat

### 1 faire bcp de modele
### 2 regarder la structure des dessins avec la courbe des pic
### 3 selection le modèle qui semble être le mieux en essayant d'avoir le minimum de groupe 
### 4 on regarde ensuite ces paramètres

predict(foo, 
        matrix(
          c(-2,1.5), nrow=1))

predict(foo, 
        matrix(
          c(-2,2), nrow=1))

points(-2.3,1.2, pch = 16, cex = 2, col =3)
points(-2.3,2, pch = 16, cex = 2, col =3)
