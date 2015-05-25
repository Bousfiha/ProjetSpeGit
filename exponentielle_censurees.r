##Loi exponentielle avec des données censurées
rm(list=ls())

lambda = 1
TailleEch = 50
##Nombre de données observées (non censurées)
R = 25
taux_censure = R/TailleEch

exp = rexp(TailleEch,lambda)

LambdaChap = taux_censure/mean(exp)


##Test d'adéquation
##Simulation loi exponentielle
##Comparaison à une loi exponentielle
##Censure de Type II

f=pexp(exp,LambdaChap)
Ui=f[order(f)]

##Cramer-Von Mises
Sum = 0

for ( i in 1:(TailleEch-R))
  Sum = Sum + (Ui[i] - (2*i-1)/(2*TailleEch))^2

CM = Sum + (TailleEch-R)/(12*TailleEch^2) + (TailleEch/3)*(Ui[TailleEch-R]-
    (TailleEch-R)/TailleEch)^3


##Anderson Darling
Sum = 0
Sum2 = 0

for ( i in 1:(TailleEch-R)) {
  Sum = Sum + (2*i-1)*(log(Ui[i])-log(1-Ui[i]))
  Sum2 = Sum2 + log(1-Ui[i]) 
}

AD = -Sum/TailleEch -2*Sum2 -(R^2*log(1-Ui[TailleEch-R])-
    (TailleEch-R)^2*log(Ui[TailleEch-R])+TailleEch^2*Ui[TailleEch-R])/TailleEch


##Kolmogorov Smirnov
K = c(1:TailleEch-R)
for ( i in 1:(TailleEch-R))
  K[i] = abs((i-0.5)/TailleEch-Ui[i])

KS = max(K) + 0.5/TailleEch


