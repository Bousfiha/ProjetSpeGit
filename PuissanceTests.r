## Ce fichier va tester la puissance des différents tests d'adéquation
## on va simuler une loi de weibull de Beta =5
## et voir le taux de rejet si on suppose que c'est une loi exp
beta = 1.5
eta = 100
n = 30

compteurKS = 0
compteurCM = 0
compteurAD = 0

#quantile à 5%
quantileKS = 1.094
quantileCM = 0.222
quantileAD = 1.321

# Modele exp
for (i in 1:1000) {
  sim <- rweibull(n,beta,eta)
  lambda <- 1/mean(sim)
  f=pexp(sim,lambda)
  Ui= f[order(f)]
  # Kolmogorov
  source("KS.R")
  Dn <- KS(Ui,n)
  D2 <- (Dn-0.2/n)*(sqrt(n)+0.26+0.5/sqrt(n))
  if (D2 > quantileKS)
    compteurKS = compteurKS +1
  
  # Cramer-von Mises
  source("CVM.R")
  W = CVM(Ui,n)
  W2 = (1+0.16/sqrt(n))*W
  if (W2 > quantileCM)
    compteurCM = compteurCM +1
  
  #Anderson-Darling
  source("AD.R")
  AD = AD(Ui,n)
  A2 = AD*(1+0.6/sqrt(n))
  if (A2 > quantileAD)
    compteurAD = compteurAD +1
  }