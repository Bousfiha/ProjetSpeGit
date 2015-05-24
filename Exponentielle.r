##Loi exponentielle

lambda = 2
TailleEch = 30
NbIt = 1000

LambdaSimu = c(1:1000)
VarSimu = c(1:1000)

for ( j in 1:NbIt) {
##Simulation de la loi
exp = rexp(TailleEch,lambda)
##Estimation du paramètre lambda
LambdaSimu[j] = 1/mean(exp)
}

LambdaSimuMoy = mean(LambdaSimu)
VarMoy= mean(LambdaSimu^2)-LambdaSimuMoy^2

exp = rexp(TailleEch,lambda)
LambdaChap = 1/mean(exp)

##Comparaison à une loi exponentielle
f = pexp(exp,LambdaChap)

##Comparaison à une loi de Weibull
##f=pweibull(exp,5,1)

Ui= f[order(f)]
##Test de Kolmogorov Smirnov
source("KS.r")
D = KS(Ui,TailleEch)
K = (D-0.2/sqrt(TailleEch))*(sqrt(TailleEch)+0.26+0.5/sqrt(TailleEch))

##Test de Cramer-von Mises
source("CVM.r")
W = CVM(Ui,TailleEch)
CM = W*(1+0.16/TailleEch)

##Test d'Anderson Darling
## Calcul de la statistique d'Anderson Darling
source("AD.r")
A = AD(Ui,TailleEch)
AD = A*(1+0.6/TailleEch)



