library(ggplot2)

#dados do descritor 1
ID <- 1:20
X1 <- c(16,16,13,12,17,11,11,11,10,14,2,7,5,3,10,9,4,4,2,4)
Y1 <- c(9,19,15,17,10,12,10,17,13,13,7,7,3,2,7,0,7,4,5,6)
descritor1 <- data.frame(id=ID, X=X1, Y=Y1)
consulta1 <- c(12,8)

#dados do descritor 2
ID <- 1:20
X2 <- c(10,18,7,13,17,8,9,9,10,14,10,3,4,9,3,7,7,8,6,7)
Y2 <- c(15,15,14,19,16,14,10,14,12,14,1,10,3,0,6,2,7,10,7,9)
descritor2 <- data.frame(id=ID, X=X2, Y=Y2)
consulta2 <- c(10,10)

#imagens relevantes
IDrel <- c(7,8,9,10,17,18,19,20)

# Calculo de Distancia L2
DistL2 <- function(x, y){
  d = 0
  for (i in c(1:length(x))) {
    d = d+(x[i]-y[i])^2
  }
  return(sqrt(d))
}

calcDists <- function(descritor, consulta) {
  dists <- c()
  for (i in 1:length(descritor$id)) {
    dists <- c(dists, DistL2(c(descritor[i,2], descritor[i,3]), consulta))
  }
  
  return(dists)
}

distsDesc1 <- order(calcDists(descritor1, consulta1))
distsDesc2 <- order(calcDists(descritor2, consulta2))

precision <- function(returned, relevant) {
  rate <- length(intersect(returned, relevant))/length(returned)
  return (rate)
}

recall <- function(returned, relevant) {
  rate <- length(intersect(returned, relevant))/length(relevant)
  return(rate)
}


points1 <- matrix(nrow=20, ncol=2)
points2 <- matrix(nrow=20, ncol=2)
for(i in 1:20) {
  points1[i,] <- c(precision(distsDesc1[1:i], IDrel), recall(distsDesc1[1:i], IDrel))
  points2[i,] <- c(precision(distsDesc2[1:i], IDrel), recall(distsDesc2[1:i], IDrel))
}

points1df <- data.frame(Descritor="Descritor1",  Precisao=points1[,1], Revocacao=points1[,2])
points2df <- data.frame(Descritor="Descritor2", Precisao=points2[,1], Revocacao=points2[,2])

pointsDf <- rbind(points1df, points2df)

g1 <- ggplot(pointsDf, aes(x=Revocacao, group = Descritor, colour = Descritor)) + geom_line(aes(y=Precisao)) + geom_point(aes(y=Precisao))
g1
