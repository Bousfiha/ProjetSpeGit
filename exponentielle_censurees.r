##Loi exponentielle avec des données censurées
rm(list=ls())

lambda = 1
TailleEch = 100
##Nombre de données observées (non censurées)
R = 50
taux_censure = R/TailleEch

exp = rexp(TailleEch,lambda)

LambdaChap = taux_censure/mean(exp)


##Test d'adéquation
##Simulation loi exponentielle
##Comparaison à une loi exponentielle
##Censure de Type II



