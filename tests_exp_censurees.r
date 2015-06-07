##Tests d'adéquation
##Simulation loi exponentielle
##Comparaison à une loi exponentielle
##Censure de Type II

rm(list=ls())

lambda = 1
TailleEch = 100
NbIt = 1000


source("quantiles_exp_cens.r")

for ( k in 1:7 ) {
  retenir_h0_cm = 0
  rejeter_h0_cm = 0
  
  retenir_h0_ad = 0
  rejeter_h0_ad = 0
  AD=0
  CM=0
  for ( i in 1:NbIt) {
    exp = rexp(TailleEch,lambda)
    expOrd = exp[order(exp)]
    LambdaChap = R[k]/(sum(expOrd[1:R[k]]) + (TailleEch-R[k])*expOrd[R[k]])
    f=pexp(exp,LambdaChap)
    Ui=f[order(f)]

    ##Cramer-Von Mises
    source("CM_cens.r")
    CM=CVM_cens(Ui,TailleEch,R[k])

    if (CM > quantile_CM[k]) 
      rejeter_h0_cm = rejeter_h0_cm + 1
    else
      retenir_h0_cm = retenir_h0_cm + 1

    ##Anderson Darling
    source("AD_cens.r")
    AD=AD_cens(Ui,TailleEch,R[k])

    if (AD > quantile_AD[k]) 
      rejeter_h0_ad = rejeter_h0_ad + 1
    else
      retenir_h0_ad = retenir_h0_ad + 1

    alpha_ad[k] = (rejeter_h0_ad/NbIt)*100
    alpha_cm[k] = (rejeter_h0_cm/NbIt)*100

##Kolmogorov Smirnov
##K = c(1:TailleEch-R)
##for ( i in 1:(TailleEch-R))
##  K[i] = abs((i-0.5)/TailleEch-Ui[i])

##KS = max(K) + 0.5/TailleEch

  }

}

plot(R,alpha_ad,pch=20,xlab="Pourcentage des données observées",ylab="Risque de première espèce",col="blue",font.axis=2,cex=1.2)
par(new=TRUE)
plot(R,alpha_cm,pch=17,xlab="Pourcentage des données observées",ylab="Risque de première espèce",col="red",font.axis=2,cex=1.2)
title("Evolution du lambda estimé en fonction de R",font.main=2)