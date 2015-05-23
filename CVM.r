##Script Calculant la statistique Wn
##du test d'adéquation de Cramer-von Mises

CVM<-function(Ui,TailleEch) {
  Sum = 0
  for (i in 1:TailleEch) {
      Sum = Sum + (Ui[i] - (2*i-1)/(2*TailleEch))^2
  }
  W = Sum + 1/(12*TailleEch)
  return(W)
}