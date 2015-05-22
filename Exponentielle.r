##Loi exponentielle

lambda = 10
TailleEch = 30
NbIt = 1000

LambdaSimu = c(1:1000)
VarSimu = c(1:1000)

for ( j in 1:NbIt) {

##Simulation de la loi
exp = rexp(TailleEch,lambda)

##Estimation du paramètre lambda
denominateur = 0
for (i in 1:TailleEch) {
	denominateur <- denominateur + exp[i]
}
LambdaSimu[j] =TailleEch/denominateur

}

LambdaSimuMoy = mean(LambdaSimu)
VarMoy= mean(LambdaSimu^2)-LambdaSimuMoy^2



exp = rexp(TailleEch,lambda)
LambdaChap = 1/mean(exp)

##Test de Kolmogorov Smirnov
TestKS = ks.test(exp, "rexp", LambdaChap)
##K = sqrt(TailleEch)*TestKS$statistic
KS = (TestKS$statistic - 0.2/TailleEch)*(sqrt(TailleEch)+0.26+0.5/sqrt(TailleEch))

##Test de Cramer-von Mises
require(dgof)
f = dexp(exp,LambdaChap)
Ui= f[order(f)]
y = stepfun(exp[order(exp)],c(0,Ui))
TestCVM = cvm.test(exp,y)
CM = TestCVM$statistic*(1+0.16/TailleEch)

##Test d'Anderson Darling
## Calcul de la statistique d'Anderson Darling
A = 0
Sum = 0
f = dexp(exp, LambdaChap)
Ui= f[order(f)]
for ( i in 1:TailleEch) {
	Sum = Sum + (2*i-1-2*TailleEch)*log(1-Ui[i])-(2*i-1)*log(Ui[i])
}
AD = (-TailleEch + Sum/TailleEch) 


