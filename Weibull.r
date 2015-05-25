## Test d'un échantillion de loi de Weibull
## Paramètres: Eta = 100       Beta =  3
## Taille Echantillion : 20
n = 30
eta = 100
beta = 3
sim <- rweibull(n,beta,eta)

## Graphe de proba pour avoir des conditions initiales
graphe <- matrix(0,n,2)
graphe[,1] = sim[order(sim)]
for (i in 1:n) {
  graphe[i,1] = log(graphe[i,1])
  graphe[i,2] = log(log(n/(n-i)))
}
graphe <- graphe[-n,]
#plot(graphe[,2]~graphe[,1])
LM = lm(graphe[,2]~graphe[,1])
abline(LM)
origine=LM$coef[1]
pente = LM$coef[2]
beta0 = pente
eta0 = exp(-origine/beta0)
## Estimation des paramètres
 f <- function(param) {
	eta = param[1]
	beta = param[2]
	-(n*log(beta) - n*beta*log(eta) + (beta-1)*sum(log(sim)) - sum((sim/eta)^beta)) 
	}

## Calcul de la moyenne des paramètres estimés
nbrExp = 1000
simEta <- c(1:nbrExp)
simBeta <- c(1:nbrExp)
for (i in 1:nbrExp) {	
	sim <- rweibull(n,beta,eta)
	res <- optim(c(eta0,beta0),f)
	simEta[i] = res$par[1]
	simBeta[i] = res$par[2]
}
etaMoy <- mean(simEta)
betaMoy <- mean(simBeta)
## Calcul de la variance des paramètre
varEta = mean(simEta^2)-etaMoy^2
varBeta = mean(simBeta^2)-betaMoy^2






## Tests d'adéquation
simOrdre <- sim[order(sim)]
simLog <- -log(sim)
simLogOrdre <- simLog[order(simLog)]
BetaEstime<-function(beta) {
  (-beta + sum(simLogOrdre)/n - sum(simLogOrdre*exp(-simLogOrdre/beta))/sum(exp(-simLogOrdre/beta)))
}
# Recherche du Beta0
#Var = mean(simLogOrdre^2) - mean(simLogOrdre)^2
#betaGumbel0 = sqrt(Var * 6/3.14^2)
#      graphe <- matrix(0,n,2)
#      graphe[,1] = simLogOrdre
#      for (i in 1:n) {
#      graphe[i,2] = log(-log(i/n))
#      }
#      graphe <- graphe[-n,]
#      plot(graphe[,2]~graphe[,1])
#      LM = lm(graphe[,2]~graphe[,1])
#      origineGumbel =LM$coef[1]
#      penteGumbel = LM$coef[2]
#      betaGumbel0 = -1/penteGumbel
resOptim = optimize(function(x) abs(BetaEstime(x)),c(0,10))
betaGumbel = resOptim$minimum
etaGumbel = -betaGumbel*log(sum(exp(-simLogOrdre/betaGumbel)/n))

f2 = c(1:30)
for (i in 1:30){
  f2[i] = pGumbel(simLogOrdre[i],betaGumbel,etaGumbel)
}
# Kolmogorov
Dn <- KS(f2,n)
D2 <- sqrt(n)*Dn

# Cramer-von Mises
source("CVM.r")
W = CVM(f2,n)
W2 = (1+0.2/sqrt(n))*W

#Anderson-Darling
AD = AD(f2,n)
A2 = AD*(1+0.2/sqrt(n))
