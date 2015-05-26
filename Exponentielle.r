##Loi exponentielle
rm(list=ls())

lambda = 1
TailleEch = 30
NbIt = 1000

LambdaSimu = c(1:1000)
VarSimu = c(1:1000)

for ( j in 1:NbIt) {
##Simulation de la loi
exp = rexp(TailleEch,lambda)
##Estimation du paramètre lambda
LambdaSimu[j] = 1/mean(exp)
}

LambdaSimuMoy = mean(LambdaSimu)
VarMoy= mean(LambdaSimu^2)-LambdaSimuMoy^2

exp = rexp(TailleEch,lambda)
LambdaChap = 1/mean(exp)

##Comparaison à une loi exponentielle
f = pexp(exp,LambdaChap)

##Comparaison à une loi de Weibull
##f=pweibull(exp,5,1)

Ui= f[order(f)]
##Test de Kolmogorov Smirnov
source("KS.r")
D = KS(Ui,TailleEch)
K = (D-0.2/sqrt(TailleEch))*(sqrt(TailleEch)+0.26+0.5/sqrt(TailleEch))

##Test de Cramer-von Mises
source("CVM.r")
W = CVM(Ui,TailleEch)
CM = W*(1+0.16/TailleEch)

##Test d'Anderson Darling
## Calcul de la statistique d'Anderson Darling
source("AD.r")
A = AD(Ui,TailleEch)
AD = A*(1+0.6/TailleEch)

##Test de puissance au seuil 5%
##Pour KS, quantile = 1.094

##quantile_KS = 1.094
##quantile_CM = 0.222
#quantile_AD = 1.321

quantile_KS = 0.895
quantile_CM = 0.126
quantile_AD = 0.752

retenir_h0_ks = 0
rejeter_h0_ks = 0

retenir_h0_cm = 0
rejeter_h0_cm = 0

retenir_h0_ad = 0
rejeter_h0_ad = 0

for ( i in 1:1000) {
  exp = rexp(TailleEch,lambda)
  ##f = pexp(exp,LambdaChap)
  f=pnorm(exp,mean(exp),var(exp))
  
  Ui= f[order(f)]
  
  D = KS(Ui,TailleEch)
  ##K = (D-0.2/TailleEch)*(sqrt(TailleEch)+0.26+0.5/sqrt(TailleEch))
  K = D*(sqrt(TailleEch)-0.01+0.85/sqrt(TailleEch))
  if (K > quantile_KS) 
    rejeter_h0_ks = rejeter_h0_ks + 1
  else
    retenir_h0_ks = retenir_h0_ks + 1
  
  W = CVM(Ui,TailleEch)
  ##CM = W*(1+0.16/TailleEch)
  CM = W*(1+0.5/TailleEch)
  if (CM > quantile_CM) 
    rejeter_h0_cm = rejeter_h0_cm + 1
  else
    retenir_h0_cm = retenir_h0_cm + 1

  source("AD.r")
  A = AD(Ui,TailleEch)
  ##AD = A*(1+0.6/TailleEch)
  AD = A*(1+0.75/TailleEch+2.25/(TailleEch^2))
  
  if (AD > quantile_AD) 
    rejeter_h0_ad = rejeter_h0_ad + 1
  else
    retenir_h0_ad = retenir_h0_ad + 1
  
}

puissance_ks = (1-retenir_h0_ks/1000)*100
puissance_ad = (1-retenir_h0_ad/1000)*100
puissance_cm = (1-retenir_h0_cm/1000)*100

