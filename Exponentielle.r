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
denominateur = 0
LambdaSimu[j] = 1/mean(exp)
}

LambdaSimuMoy = mean(LambdaSimu)
VarMoy= mean(LambdaSimu^2)-LambdaSimuMoy^2

exp = rexp(TailleEch,lambda)
LambdaChap = 1/mean(exp)

f = pexp(exp,LambdaChap)
##f = pexp(exp)
Ui= f[order(f)]

##Test de Kolmogorov Smirnov
K1=c(1:TailleEch)
K2=K1

for (i in 1:TailleEch) {
  K1[i] = i/TailleEch - Ui[i]
  K2[i] = Ui[i] - (i-1)/TailleEch
}
  Max1 = max(K1)
  Max2 = max(K2)

D = max(K1,K2)
K = (D-0.2/sqrt(TailleEch))*(sqrt(TailleEch)+0.26+0.5/sqrt(TailleEch))



##Test de Cramer-von Mises
require(dgof)

y = stepfun(exp[order(exp)],c(0,Ui))
TestCVM = cvm.test(exp,y)
CM = TestCVM$statistic*(1+0.16/TailleEch)


##Test d'Anderson Darling
## Calcul de la statistique d'Anderson Darling
Sum = 0
for ( i in 1:TailleEch) {
	Sum = Sum + (2*i-1-2*TailleEch)*log(1-Ui[i])-(2*i-1)*log(Ui[i])
}
AD = (-TailleEch + Sum/TailleEch) 
##AD


