##Test d'adéquation d'Anderson Darlin
##Pour des données censurées

AD_cens<-function(Ui,TailleEch,R) {
  Sum = 0
  Sum2 = 0
  for ( i in 1:R) {
    Sum = Sum + (2*i-1)*(log(Ui[i])-log(1-Ui[i]))
    Sum2 = Sum2 + log(1-Ui[i]) 
  }
  
  AD = -Sum/TailleEch -2*Sum2 -((TailleEch-R)^2*log(1-Ui[R])-
                                  R^2*log(Ui[R])+TailleEch^2*Ui[R])/TailleEch
  return(AD)
}