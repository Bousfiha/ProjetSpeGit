##Script Calculant la statistique 
##du test d'adéquation d'Anderson Darling

AD<-function(Ui,TailleEch) {
  Sum = 0
  for ( i in 1:TailleEch) {
    Sum = Sum + (2*i-1-2*TailleEch)*log(1-Ui[i])-(2*i-1)*log(Ui[i])
  }
  A = (-TailleEch + Sum/TailleEch)
  return(A)
}