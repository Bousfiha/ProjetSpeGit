## Test d'un échantillion de loi de Weibull
## Paramètres: Eta = 100       Beta =  3
## Taille Echantillion : 50
n = 50
eta = 100
beta = 3
Itr = 1000

compteurKS = 0
compteurCM = 0
compteurAD = 0

#quantile à 5%
quantileKS = 0.856
quantileCM = 0.124
quantileAD = 0.757

for (i in 1:Itr) {
## Tests d'adéquation
sim <- rweibull(n,beta,eta)
simLog <- -log(sim)
simLogOrdre <- simLog[order(simLog)]
BetaEstime<-function(beta) {
  (-beta + sum(simLogOrdre)/n - sum(simLogOrdre*exp(-simLogOrdre/beta))/sum(exp(-simLogOrdre/beta)))
}

resOptim = optimize(function(x) abs(BetaEstime(x)),c(0,10))
betaGumbel = resOptim$minimum
etaGumbel = -betaGumbel*log(sum(exp(-simLogOrdre/betaGumbel)/n))

f2 = pGumbel(simLogOrdre,betaGumbel,etaGumbel)

# Kolmogorov
source("KS.R")
Dn <- KS(f2,n)
D2 <- sqrt(n)*Dn
if (D2 > quantileKS)
  compteurKS = compteurKS +1

# Cramer-von Mises
source("CVM.r")
W = CVM(f2,n)
W2 = (1+0.2/sqrt(n))*W
if (W2 > quantileCM)
  compteurCM = compteurCM +1

#Anderson-Darling
source("AD.r")
AD = AD(f2,n)
A2 = AD*(1+0.2/sqrt(n))
if (A2 > quantileAD)
  compteurAD = compteurAD +1

}

#Pourcentages
AlphaKS = 100*compteurKS/Itr
AlphaCM = 100*compteurCM/Itr
AlphaAD = 100*compteurAD/Itr