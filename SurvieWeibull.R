## On simule un echantillion de loi we Weibull
beta = 3
eta = 100
n = 100
R = 90
Int = 200
## Fonction de survie reel
sim = rweibull(n,beta,eta)
sim = sim[order(sim)]
graphe1 = matrix(0,Int,2)
for (i in 1:Int){
  graphe1[i,1] = i
  graphe1[i,2] = 1 - pweibull(i,beta,eta)
}
plot(graphe1, col="red", type="l")

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
## Fontion pour estimer les paramètres
f <- function(param) {
  eta = param[1]
  beta = param[2]
  -(R*log(beta) - R*beta*log(eta) + (beta-1)*sum(log(simOrdre)[1:R]) - (1/(eta^beta))*(sum((simOrdre^beta)[1:R])+(n-R)*simOrdre[R]^beta)) 
}
## Graphe de probabilite pour estimer les param initiaux de l'optim
graphe <- matrix(0,R,2)
for (k in 1:R) {
  graphe[k,1] = log(sim[k])
  Fti = (k-0.3)/(n+0.4)
  graphe[k,2] = log(log(1/(1-Fti)))
}
if (R==n)
  graphe <- graphe[-n,]
LM = lm(graphe[,2]~graphe[,1])
origine=LM$coef[1]
pente = LM$coef[2]
eta0 = exp(-origine/pente)
beta0 = pente
res <- optim(c(eta0,beta0),f)
simEta = res$par[1]
simBeta = res$par[2]

graphe3 = matrix(0,Int,2)
for (i in 1:Int){
  graphe3[i,1] = i
  graphe3[i,2] = 1 - pweibull(i,simBeta,simEta)
}
par(new=TRUE)
plot(graphe3, col="green", type="l")

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
