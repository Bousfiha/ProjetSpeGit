##Test de puissance 
##Simulation échantillon Weibull
##Comparaison à une loi exponentielle

rm(list=ls())

beta = 1.5
eta = 100
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
    exp = rweibull(TailleEch,beta,eta)
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
    
    
    puissance_ad[k] = (1-retenir_h0_ad/NbIt)*100
    puissance_cm[k] = (1-retenir_h0_cm/NbIt)*100
    
    ##Kolmogorov Smirnov
    ##K = c(1:TailleEch-R)
    ##for ( i in 1:(TailleEch-R))
    ##  K[i] = abs((i-0.5)/TailleEch-Ui[i])
    
    ##KS = max(K) + 0.5/TailleEch
    
  }
}