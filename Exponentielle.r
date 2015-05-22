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

##Test de Kolmogorov Smirnov
TestKS = ks.test(exp, "pexp", LambdaChap)
KS = (TestKS$statistic - 0.2/TailleEch)*(sqrt(TailleEch)+0.26+0.5/sqrt(TailleEch))
##TestKS = ks.test(exp,"rexp")
##KS = (sqrt(TailleEch)+0.12+0.11/sqrt(TailleEch))*TestKS$statistic


##Test de Cramer-von Mises
require(dgof)
f = pexp(exp,LambdaChap)
##f = pexp(exp)
Ui= f[order(f)]
y = stepfun(exp[order(exp)],c(0,Ui))
TestCVM = cvm.test(exp,y)
CM = TestCVM$statistic*(1+0.16/TailleEch)
##CM = (TestCVM$statistic-0.4/TailleEch+0.6/TailleEch^2)*(1+1/n)


##Test d'Anderson Darling
## Calcul de la statistique d'Anderson Darling
A = 0
Sum = 0
for ( i in 1:TailleEch) {
	Sum = Sum + (2*i-1-2*TailleEch)*log(1-Ui[i])-(2*i-1)*log(Ui[i])
}
AD = (-TailleEch + Sum/TailleEch) 
##AD


