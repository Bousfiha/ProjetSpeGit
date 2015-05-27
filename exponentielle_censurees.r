##Loi exponentielle avec des données censurées
rm(list=ls())

lambda = 2
TailleEch = 100
NbIt = 1000

LambdaSimu = c(1:1000)
LambdaSimuMoy = c(1:100)
VarMoy = c(1:100)
##Nombre de données observées (non censurées)

for ( R in 1:TailleEch) {


for ( j in 1:NbIt) {
  ##Simulation de la loi
  exp = rexp(TailleEch,lambda)
  expOrd = exp[order(exp)]
  ##Estimation du paramètre lambda
  Sum = 0 
  LambdaSimu[j] = R/(sum(expOrd[1:R]) + (TailleEch-R)*expOrd[R])
}

LambdaSimuMoy[R] = mean(LambdaSimu)
VarMoy[R]= mean(LambdaSimu^2)-LambdaSimuMoy[R]^2

}

plot(c(1:TailleEch),LambdaSimuMoy,xlab="R",ylab="LambdaMoy",col="red")
title("Evolution de LambdaMoy en fonction de R")

plot(c(1:TailleEch),VarMoy,xlab="R",ylab="VarMoy",col="blue")
title("Evolution de VarMoy en fonction de R")
