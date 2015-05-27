##Test d'adéquation de Cramer-von Mises
##Pour des données censurées

CVM_cens<-function(Ui,TailleEch,R) {
  Sum = 0    
  for ( i in 1:R )
    Sum = Sum + (Ui[i] - (2*i-1)/(2*TailleEch))^2
  
  CM = Sum + R/(12*TailleEch^2) + (TailleEch/3)*(Ui[R]-R/TailleEch)^3
  return(CM)
}