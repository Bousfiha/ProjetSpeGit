## On simule un echantillion de loi we Weibull
beta = 3
eta = 100
n = 100
Int = 1000
## Fonction de survie reel
sim = rweibull(n,beta,eta)
sim = sim[order(sim)]
graphe1 = matrix(0,Int,2)
for (i in 1:Int){
  graphe1[i,1] = i
  graphe1[i,2] = 1 - pweibull(i,beta,eta)
}
plot(graphe1, col="red")

## Fonction de survie empirique
graphe2 = matrix(0,Int,2)
Indicatrice = matrix(0,n,1)
for (i in 1:Int){
  graphe2[i,1] = i
  for (j in 1:n) {
    if (sim[j] > i)
      Indicatrice[j] = 1
  }
  graphe2[i,2] = (1/n)*sum(Indicatrice)
  Indicatrice = matrix(0,n,1)
}
par(new=TRUE)
plot(graphe2, col="blue")