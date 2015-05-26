## Simulation d'une loi de weibull 
## avec observations censurées

beta = 5
eta = 100
n = 100
#Taux de censure
R = 30
taux_censure = R/n
# Choix aléatoire des temps censurés:
repeat{
sigma <- rbinom(n,1,(n-R)/n)
if (sum(sigma)==(n-R))
  break;
}
sim = matrix(0,n,2)
sim[,1] = rweibull(n,beta,eta)
sim[,2] = sigma

## Graphe de probabilite
graphe <- matrix(0,R,2)
simOrdre = sim[order(sim[,1]),]
j=1
for (i in 1:n) {
  if (simOrdre[i,2]==0)  {
    graphe[j,1] = log(simOrdre[i,1])
    Fti = (j-0.3)/(n+0.4)
    graphe[j,2] = log(log(1/(1-Fti))/log(10))
    j = j+1
  }
}
plot(graphe[,2]~graphe[,1])
LM = lm(graphe[,2]~graphe[,1])
abline(LM)
