## Simulation d'un echantillion censuré de type III (Multicensur�)
simWeibIII <- function(n,beta,eta)
{
res = matrix(0,n,2)
sim = rweibull(n,beta,eta)
Tc = rweibull(n,beta,eta*0.9)
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
RInit = n/10
lambdaBetaRMoy = matrix(0,n,3)
lambdaBetaRVar = matrix(0,n,3)

taux_censure = R/n
Itr = 1000
## Calcul de la moyenne et variance des estimations de paramètres
  sim = simWeibIII(n,beta,eta)
  R = sum(Ech[,2])
  simOrdre = sim[order(sim[,2], decreasing=TRUE),]
  ## Fontion pour estimer les paramètres
  f <- function(param) {
    eta = param[1]
    beta = param[2]
    -(R*log(beta) - R*beta*log(eta) + (beta-1)*sum(log(simOrdre)[1:R]) - (1/(eta^beta))*(sum((simOrdre^beta)[1:R])+(n-R)*simOrdre[R]^beta)) 
  }
  ## Graphe de probabilite pour estimer les param initiaux de l'optim
  graphe4 = matrix(1,R,2)
  for (i in 1:R){
   # if (simOrdre[i,2]==1) {
      graphe4[i,1] = simOrdre[i]
      for (j in 1:R) {
          if (simOrdre[j]>simOrdre[i]) {
            break
          }
          graphe4[i,2] = graphe4[i,2]*(n-j)/(n-j+1)
      }
  }
  graphe4[,2]= log(log(1/(1 - graphe4[,2])))

#par(new=TRUE)
#plot(graphe4, type="S", ylim=c(0:1))




#Graphes d'eta
#pdf("VarFonctionDeRII.pdf", height=5,width=10)
#par(mfrow = c(1,2))
#plot(lambdaBetaRMoy[RInit:100,], main="Estimation de Eta en fonction de r",xlab="R", ylab="moy(Eta)", col="blue", pch=16)

#plot(lambdaBetaRVar[RInit:100,], main="Variance des estimation de Eta en fonction de r",xlab="R", ylab="Var(Eta)", col="red", pch=16)
#Graphes de Beta
#plot(lambdaBetaRMoy[RInit:100,-2], main="Estimation de Beta en fonction de r",xlab="R", ylab="moy(Beta)", col="blue", pch=16)
#plot(lambdaBetaRVar[RInit:100,-2], main="Variance de Beta en fonction de r", xlab="R", ylab="Var(Beta)", col="red",  pch=16)
#dev.off()