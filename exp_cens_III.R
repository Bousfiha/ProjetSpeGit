##Simulation d'un échantillon censuré de type III
##de loi exponentielle
simExpIII <- function(TailleEch,lambda)
{
  res = matrix(0,TailleEch,2)
  sim = rexp(TailleEch,lambda)
  Tc = rexp(TailleEch,lambda)
  for (i in 1:TailleEch) {
    if (Tc[i]<sim[i]) {
      res[i,2] = 0
      res[i,1] = Tc[i]
    } else {
      res[i,2] = 1
      res[i,1] = sim[i]
    }   
  }
  return(res)
}


TailleEch=100
lambda = 1
NbIt = 1000
LambdaChap = c(1:NbIt)
R = c(1:NbIt)
for ( i in 1:NbIt) {
  exp = simExpIII(TailleEch,lambda)
  ##données observées
  R[i] = sum(exp[,2])
  LambdaChap[i] = sum(exp[,2])/sum(exp[1,])  
}


plot(R,LambdaChap)