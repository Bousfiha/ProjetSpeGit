## Ce fichier va tester la puissance des différents tests d'adéquation
## on va simuler une loi de weibull de Beta =5
## et voir le taux de rejet si on suppose que c'est une loi exp
beta = 5
eta = 100
n = 30


# Modele exp
for (i in 1:1000)
  sim <- rweibull(n,beta,eta)
  lambda <- 1/mean(sim)
