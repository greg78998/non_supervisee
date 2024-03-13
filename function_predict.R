distaucentre <- function(x,centre){
  k=nrow(centre)
  choix <- 1:k
  for (ii in 1:k){
    choix[ii] <- sum((x-centre[ii,])^2)
  }
  return(which.min(choix))
}

aff <- 1:nrow(donTT)
CENTRE <- gp3$centers 

for (ii in 1:nrow(donTT)){
  aff[ii] <- distaucentre(donTT[ii,-1], )
}