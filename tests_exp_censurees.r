##Tests d'adéquation
##Simulation loi exponentielle
##Comparaison à une loi exponentielle
##Censure de Type II

rm(list=ls())

lambda = 1
TailleEch = 100
NbIt = 1000

##données observées
R = 20



##Quantiles au seuil 5%
quantile_CM = 0.017
quantile_AD = 0.252

retenir_h0_cm = 0
rejeter_h0_cm = 0

retenir_h0_ad = 0
rejeter_h0_ad = 0


for ( i in 1:NbIt) {
exp = rexp(TailleEch,lambda)
expOrd = exp[order(exp)]
LambdaChap = R/(sum(expOrd[1:R]) + (TailleEch-R)*expOrd[R])
f=pexp(exp,LambdaChap)
Ui=f[order(f)]

##Cramer-Von Mises
Sum = 0

for ( i in 1:R )
  Sum = Sum + (Ui[i] - (2*i-1)/(2*TailleEch))^2

CM = Sum + R/(12*TailleEch^2) + (TailleEch/3)*(Ui[R]-R/TailleEch)^3

if (CM > quantile_CM) 
  rejeter_h0_cm = rejeter_h0_cm + 1
else
  retenir_h0_cm = retenir_h0_cm + 1

##Anderson Darling
Sum = 0
Sum2 = 0

for ( i in 1:R) {
  Sum = Sum + (2*i-1)*(log(Ui[i])-log(1-Ui[i]))
  Sum2 = Sum2 + log(1-Ui[i]) 
}

AD = -Sum/TailleEch -2*Sum2 -((TailleEch-R)^2*log(1-Ui[R])-
                                R^2*log(Ui[R])+TailleEch^2*Ui[R])/TailleEch

if (AD > quantile_AD) 
  rejeter_h0_ad = rejeter_h0_ad + 1
else
  retenir_h0_ad = retenir_h0_ad + 1


alpha_ad = (rejeter_h0_ad/NbIt)*100
alpha_cm = (rejeter_h0_cm/NbIt)*100

##Kolmogorov Smirnov
##K = c(1:TailleEch-R)
##for ( i in 1:(TailleEch-R))
##  K[i] = abs((i-0.5)/TailleEch-Ui[i])

##KS = max(K) + 0.5/TailleEch

}
