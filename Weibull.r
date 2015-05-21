## Test d'un échantillion de loi de Weibull
## Paramètres: Eta = 1       Beta =  5
## Taille Echantillion : 20
n = 30
eta = 1
beta = 5
sim <- rweibull(n,beta,eta)

## Estimation des paramètres
 f <- function(param) {
	eta = param[1]
	beta = param[2]
	-(n*log(beta) - n*beta*log(eta) + (beta-1)*sum(log(sim)) - sum((sim/eta)^beta)) 
	}

## Calcul de la moyenne des paramètres estimés
nbrExp = 1000
simEta <- c(1:nbrExp)
simBeta <- c(1:nbrExp)
for (i in 1:nbrExp) {	
	sim <- rweibull(n,beta,eta)
	res <- optim(c(0.1,0.1),f)
	simEta[i] = res$par[1]
	simBeta[i] = res$par[2]
}
etaMoy <- mean(simEta)
betaMoy <- mean(simBeta)
## Calcul de la variance des paramètre
varEta = mean(simEta^2)-etaMoy^2
varBeta = mean(simBeta^2)-betaMoy^2

## Tests d'adéquation
# Kolmogorov
res <- ks.test(sim,"pweibull",beta,eta)
K = sqrt(n)*res$statistic[1]
# Cramer-von Mises
f = c(1:30)
simOrdre = sim[order(sim)]

for (i in 1:30)
  f[i] = pweibull(simOrdre[i],beta,eta)
funct = stepfun(simOrdre,c(0,f))
cvm.test(sim,funct)