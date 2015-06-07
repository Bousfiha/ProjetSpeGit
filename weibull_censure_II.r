## Simulation d'une loi de weibull 
## avec observations censur√©es

beta = 3
eta = 100
n = 100
RInit = n/10
lambdaBetaRMoy = matrix(0,n,3)
lambdaBetaRVar = matrix(0,n,3)
#for (R in RInit:n) { 
R=80
#Taux de censure
#R = 70
taux_censure = R/n
Itr = 1000
## Calcul de la moyenne et variance des estimations de param√®tres

simBeta = c(1:Itr)
simEta = c(1:Itr)
for (i in 1:Itr) {
  sim = rweibull(n,beta,eta)
  simOrdre = sim[order(sim)]
  ## Fontion pour estimer les param√®tres
  f <- function(param) {
    eta = param[1]
    beta = param[2]
    -(R*log(beta) - R*beta*log(eta) + (beta-1)*sum(log(simOrdre)[1:R]) - (1/(eta^beta))*(sum((simOrdre^beta)[1:R])+(n-R)*simOrdre[R]^beta)) 
  }
  ## Graphe de probabilite pour estimer les param initiaux de l'optim
  graphe <- matrix(0,R,2)
  simOrdre = sim[order(sim)]
  for (k in 1:R) {
    graphe[k,1] = log(simOrdre[k])
    Fti = (k-0.3)/(n+0.4)
    #Fti = k/n
    graphe[k,2] = log(log(1/(1-Fti)))
  }
  if (R==n)
    graphe <- graphe[-n,]
  pdf("GrapheProbaWeibullII.pdf", height=6,width=6)
  plot(graphe[,2]~graphe[,1], xlab="", ylab="", main="Graphe de probabilitÈ de l'Èchantillon censurÈ (Type II)", col="green", pch=16)
  dev.off()
  LM = lm(graphe[,2]~graphe[,1])
  origine=LM$coef[1]
  pente = LM$coef[2]
  eta0 = exp(-origine/pente)
  beta0 = pente
  res <- optim(c(eta0,beta0),f)
  simEta[i] = res$par[1]
  simBeta[i] = res$par[2]  
}
etaMoy <- mean(simEta)
betaMoy <- mean(simBeta)
## Calcul de la variance des param√®tre
varEta = mean(simEta^2)-etaMoy^2
varBeta = mean(simBeta^2)-betaMoy^2

lambdaBetaRMoy[R,1] = R
lambdaBetaRMoy[R,2] = etaMoy
lambdaBetaRMoy[R,3] = betaMoy

lambdaBetaRVar[R,1] = R
lambdaBetaRVar[R,2] = varEta
lambdaBetaRVar[R,3] = varBeta
#}
#Graphes d'eta
pdf("VarFonctionDeRII.pdf", height=5,width=10)
par(mfrow = c(1,2))
#plot(lambdaBetaRMoy[RInit:100,], main="Estimation de Eta en fonction de r",xlab="R", ylab="moy(Eta)", col="blue", pch=16)

plot(lambdaBetaRVar[RInit:100,], main="Variance des estimation de Eta en fonction de r",xlab="R", ylab="Var(Eta)", col="red", pch=16)
#Graphes de Beta
#plot(lambdaBetaRMoy[RInit:100,-2], main="Estimation de Beta en fonction de r",xlab="R", ylab="moy(Beta)", col="blue", pch=16)
plot(lambdaBetaRVar[RInit:100,-2], main="Variance de Beta en fonction de r", xlab="R", ylab="Var(Beta)", col="red",  pch=16)
dev.off()
