##Loi exponentielle
##Données censurées de type I

rm(list=ls())

lambda = 2
TailleEch = 100
NbIt = 1000

table = matrix(0,TailleEch,2)
table_moy = table


  for ( j in 1:NbIt) { 
    R = 0
   ##Simulation de la loi
    exp = rexp(TailleEch,lambda)
    expOrd = exp[order(exp)]
    tc = runif(1,expOrd[1],expOrd[TailleEch])
    for ( i in 1:TailleEch ) {
      if ( exp[i] <= tc)
        R = R + 1
    }
  
    ##Estimation du paramètre lambda
    LambdaSimu = -(1/tc)*log(1-R/TailleEch)
    table[R,1] = table[R,1] + LambdaSimu
    table[R,2] = table[R,2] + 1
}

for ( i in 1:TailleEch) {
  if ( table[i,2] != 0)
    table[i,1] = table[i,1]/table[i,2]
  
}
table = table[table[,2]>0,]

##Tests d'adéquation


R=0
##Simulation de la loi
exp = rexp(TailleEch,lambda)
expOrd = exp[order(exp)]
tc = runif(1,expOrd[1],expOrd[TailleEch])
for ( i in 1:TailleEch ) {
  if ( expOrd[i] <= tc)
    R = R + 1
}
expObs = expOrd[1:R]
Z = c(1:R)
Z[1]= TailleEch*expObs[1]
  for ( i in 2:R) {
      Z[i] = ( R-i+1)(expObs[i]-expObs[i-1])
  }

