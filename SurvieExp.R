## On simule un echantillion de loi we Weibull
Lambda = 0.1
n = 100
R = 50
Int = 40
## Fonction de survie reel
sim = rexp(n,Lambda)
sim = sim[order(sim)]
graphe1 = matrix(0,Int,2)
for (i in 1:Int){
  graphe1[i,1] = i
  graphe1[i,2] = 1 - pexp(i,Lambda)
}
plot(graphe1, col="red", type="l", ylim=c(0:1))

## Fonction de survie empirique
graphe2 = matrix(0,Int,2)
Indicatrice = matrix(0,n,1)
for (i in 1:Int){
  graphe2[i,1] = i
  for (j in 1:R) {
    if (sim[j] > i)
      Indicatrice[j] = 1
  }
  graphe2[i,2] = (1/(n-0.3))*(sum(Indicatrice)+0.4)
  Indicatrice = matrix(0,n,1)
}
par(new=TRUE)
plot(graphe2, col="blue", type="S", ylim=c(0:1) )

## Fonction de survie avec paramètres estimés
simLambda = R/(sum(sim[1:R]) + (n-R)*sim[R])
graphe3 = matrix(0,Int,2)
for (i in 1:Int){
  graphe3[i,1] = i
  graphe3[i,2] = 1 - pexp(i,simLambda)
}
par(new=TRUE)
plot(graphe3, col="green", type="l", ylim=c(0:1))

# Fonction de survie avec Kaplan Meyer
graphe4 = matrix(1,Int,2)
for (i in 1:Int){
  graphe4[i,1] = i
  for (j in 1:R) {
    if (sim[j]>i) {
      break
    }
    graphe4[i,2] = graphe4[i,2]*(n-j)/(n-j+1)
  }
}
par(new=TRUE)
plot(graphe4, type="S", ylim=c(0:1))
