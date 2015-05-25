EstimeWeibull<-function(sim) {
  ## Graphe de proba pour avoir des conditions initiales
  graphe <- matrix(0,n,2)
  graphe[,1] = sim[order(sim)]
  for (i in 1:n) {
    graphe[i,1] = log(graphe[i,1])
    graphe[i,2] = log(log(n/(n-i)))
  }
  graphe <- graphe[-n,]
  #plot(graphe[,2]~graphe[,1])
  LM = lm(graphe[,2]~graphe[,1])
  #abline(LM)
  origine=LM$coef[1]
  pente = LM$coef[2]
  beta0 = pente
  eta0 = exp(-origine/beta0)
  ## Estimation des paramètres
  f <- function(param) {
    eta = param[1]
    beta = param[2]
    -(n*log(beta) - n*beta*log(eta) + (beta-1)*sum(log(sim)) - sum((sim/eta)^beta)) 
  }
  
  res <- optim(c(eta0,beta0),f)
  return(res)
  
}