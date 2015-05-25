##Script Calculant la statistique Dn
##du test d'adéquation Kolmogorov Smirnov
 
  
KS<-function(Ui,TailleEch) {
  K1 <- c(1:TailleEch)
  K2 <- K1
  for (i in 1:TailleEch) {
    K1[i] = i/TailleEch - Ui[i]
    K2[i] = Ui[i] - (i-1)/TailleEch
  }
  Max1 = max(K1)
  Max2 = max(K2)
    
  D = max(K1,K2)
    
  return(D)
  }