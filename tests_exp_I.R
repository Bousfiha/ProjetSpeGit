##Test d'adéquation censure type I 
##Comparaison exp exp
rm(list=ls())

lambda = 2
TailleEch = 100
Itr = 1000


#quantile à 5%
quantileKS = 1.094
quantileCM = 0.222
quantileAD = 1.321
eta = 0.5
beta = 1
R=c(1:Itr)
P = seq(from=0.1,to=1,by=0.1)
BetaKS = c(1:10)
BetaCM = BetaKS
BetaAD = BetaCM
#tc = eta*exp(log(-log(0.7))/beta)

for ( p in 1:10) {
  
  compteurKS = 0
  compteurCM = 0
  compteurAD = 0
  tc = -log(1-P[p])/lambda
  for (k in 1:Itr) {
    
    R[k]=0
    #exp = rweibull(TailleEch,beta,eta)
    exp = rexp(TailleEch,lambda)
    expOrd = exp[order(exp)]
    ##tc = runif(1,expOrd[1],expOrd[TailleEch])
    for ( i in 1:TailleEch ) {
      if ( expOrd[i] <= tc)
        R[k] = R[k] + 1
    }
    Z = c(1:R[k])
    Z[1]= TailleEch*expOrd[1]
    if ( R[k] > 1) {
      for ( j in 2:R[k]) {
        Z[j] = (R[k]-j+1)*(expOrd[j]-expOrd[j-1])
      }
    }
    n = R[k]
    
    if ( n > 0) {
      lambdachap <- 1/mean(Z)
      f=pexp(Z,lambdachap)
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
  }
  
  BetaKS[p] = 100*compteurKS/Itr
  BetaCM[p] = 100*compteurCM/Itr
  BetaAD[p] = 100*compteurAD/Itr
}