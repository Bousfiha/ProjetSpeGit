##Loi exponentielle
##Données censurées de type I

rm(list=ls())

lambda = 1
TailleEch = 100
NbIt = 1000
table = matrix(0,81,2)
Var_2 = table

P=seq(from=0.1,to=0.9,by=0.01)
for ( p in 1:81) {
  tc = -log(1-P[p])/lambda
  
  for ( j in 1:NbIt) { 
    R = 0
    ##Simulation de la loi
    exp = rexp(TailleEch,lambda)
    expOrd = exp[order(exp)]
    for ( i in 1:TailleEch ) {
      if ( exp[i] <= tc)
        R = R + 1
    }    
    
      if ( R == P[p]*TailleEch) {
      ##Estimation du paramètre lambda
      #LambdaSimu = -(1/tc)*log(1-R/TailleEch)
      LambdaSimu = R/ (sum(expOrd[1:R])+(TailleEch-R)*tc)
      Var_2[p,1] = table[p,1] + LambdaSimu^2
      table[p,1] = table[p,1] + LambdaSimu
      table[p,2] = table[p,2] + 1
      }
  }  
}
Var = Var_2[,1]/table[,2]- (table[,1]/table[,2])^2
table[,1] = table[,1]/table[,2]

plot(P,Var,pch=20,xlab="R",ylab="Variance",col="blue",font.axis=2)
title("Evolution de la variance en fonction de R",font.main=2)
#plot(P,table[,1],ylim=c(0.9,1.1),pch=20,xlab="R",ylab="Moyenne du Lambda estimé",col="blue",font.axis=2)
#title("Evolution de l'espérance du lambda estimé en fonction de R",font.main=2)

#abline(h=lambda,col="red",font.main=2)
