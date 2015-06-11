## Simulation d'un echantillion censuré de type III (Multicensuré)
simWeibIII <- function(n,beta,eta,facteur)
{
res = matrix(0,n,2)
sim = rweibull(n,beta,eta)
Tc = rweibull(n,beta,eta*facteur)
for (i in 1:n) {
  if (Tc[i]<sim[i]) {
    res[i,2] = 0
    res[i,1] = Tc[i]
  } else {
    res[i,2] = 1
    res[i,1] = sim[i]
  }   
}
return(res)
}



beta = 3
eta = 100
n = 100
Facteurs = seq(0.01,3.2,0.01)
All = matrix(1,100,3) # première colonne Eta
                     # deuxième colonne Beta
                     # troisième colonne: nombre de sim
k=1
## Calcul de la moyenne et variance des estimations de paramètres
for (Facteur in Facteurs) {
  for (h in 1:10) {
    sim = simWeibIII(n,beta,eta,Facteur)
    R = sum(sim[,2])
    if (R<n/10)
      break
    simOrdre = sim[order(sim[,2], decreasing=TRUE),]
    simObserve = simOrdre[1:R,1]
    simObserve = simObserve[order(simObserve)]
    ## Fontion pour estimer les paramètres
    f <- function(param) {
      eta = param[1]
      beta = param[2]
      -(R*log(beta) - R*beta*log(eta) + (beta-1)*sum(log(simOrdre)[1:R]) - (1/(eta^beta))*(sum((simOrdre^beta)[1:R])+(n-R)*simOrdre[R]^beta)) 
    }
    ## Graphe de probabilite pour estimer les param initiaux de l'optim
    graphe4 = matrix(1,R,2)
    for (i in 1:R){
      graphe4[i,1] = log(simObserve[i])
      #Kaplan Meier
      for (j in 1:R) {
          if (simObserve[j]>simObserve[i]) {
            break
          }
          graphe4[i,2] = graphe4[i,2]*(n-j)/(n-j+1)
      }
    }
    graphe4[,2]= log(log(1/(graphe4[,2])))
    if (R==n) {
      graphe4 <- graphe4[-n,]
    }

  #par(new=TRUE)
  #plot(graphe4, col="blue", pch=16)

  LM = lm(graphe4[,2]~graphe4[,1])
  #abline(LM)
  origine=LM$coef[1]
  pente = LM$coef[2]
  eta0 = exp(-origine/pente)
  beta0 = pente
  #Calcul de Eta et Beta
  res <- optim(c(eta0,beta0),f)
  EtaChapeau = res$par[1]
  BetaChapeau = res$par[2]
  All[R,1] = All[R,1] + EtaChapeau
  All[R,2] = All[R,2] + BetaChapeau
  All[R,3] = All[R,3] + 1
  }
}

All = All/All[,3]
#Graphes d'eta
pdf("EtaFonctionDeRIII.pdf", height=5,width=10)
#par(mfrow = c(1,2))
plot(All[10:100,1]~c(10:100), main="Estimation de Eta en fonction de r",xlab="R", ylab="Eta", col="blue", pch=16)
abline(h = eta, col = "red", lty = "dotted",lwd=4)
dev.off()
#plot(lambdaBetaRVar[RInit:100,], main="Variance des estimation de Eta en fonction de r",xlab="R", ylab="Var(Eta)", col="red", pch=16)
#Graphes de Beta
pdf("BetaFonctionDeRIII.pdf", height=5,width=10)
plot(All[10:100,2]~c(10:100), main="Estimation de Beta en fonction de r",xlab="R", ylab="Beta", col="red", pch=16)
abline(h = beta, col = "blue", lty="dotted", lwd=4)
#plot(lambdaBetaRVar[RInit:100,-2], main="Variance de Beta en fonction de r", xlab="R", ylab="Var(Beta)", col="red",  pch=16)
dev.off()

## Tests d'adéquation
