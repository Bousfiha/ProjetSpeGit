## Simulation d'un echantillon censuré  de type I de loi de Weibull
## Eta = 100, Beta = 3, n = 100

n=100
beta=3
eta=100
sim = rweibull(n,beta,eta)
sim = sort(sim)
#Simulation d'un Tc qui sera fixé par la suite


Itr = 1000

All = matrix(1,100,3) # première colonne Eta
                      # deuxième colonne Beta
                      # troisième colonne: nombre de sim

p = seq(0.2,0.9,0.01)
#p = 0.9
for (h in p) {
  tc = eta*exp(log(-log(h))/beta)
  for (i in 1:Itr) {
    R=0
    sim = rweibull(n,beta,eta)
    sim = sort(sim)

    #Calcul de R
    for (j in 1:n) {
      if (sim[j]>tc)
        R = R+1
    }
    if(R != (100*h) )
      break
    #Estimation des paramètres Eta et Beta
    f <- function(param) {
      eta = param[1]
      beta = param[2]
      -(R*log(beta) - R*beta*log(eta) + (beta-1)*sum(log(sim)[1:R]) - (1/(eta^beta))*(sum((sim^beta)[1:R])+(n-R)*sim[R]^beta)) 
    }
    ## Graphe de probabilite pour estimer les param initiaux de l'optim
    graphe <- matrix(0,R,2)
    for (k in 1:R) {
      graphe[k,1] = log(sim[k])
      Fti = (k-0.3)/(n+0.4)
      #Fti = k/n
      graphe[k,2] = log(log(1/(1-Fti)))
    }
    if (R==n)
      graphe <- graphe[-n,]
    #pdf("GrapheProbaWeibullI.pdf", height=6,width=6)
    #plot(graphe[,2]~graphe[,1], xlab="", ylab="", main="Graphe de probabilité de l'échantillon censuré (Type II)", col="green", pch=16)
  # dev.off()
    LM = lm(graphe[,2]~graphe[,1])
    origine=LM$coef[1]
    pente = LM$coef[2]
    eta0 = exp(-origine/pente)
    beta0 = pente
    res <- optim(c(eta0,beta0),f)
    EtaChapeau = res$par[1]
    BetaChapeau = res$par[2]
    All[R,1] = All[R,1] + EtaChapeau
    All[R,2] = All[R,2] + BetaChapeau
    All[R,3] = All[R,3] + 1
  }
}
All = All/All[,3]
All = All[-100,]
"
#Graphes d'eta
pdf("EtaFonctionDeRIFinal2.pdf", height=5,width=10)
#par(mfrow = c(8,1))
par(new=TRUE)
plot(All8[10:97,1]~c(10:97), main="Estimation de Eta en fonction de r",xlab="R", ylab="Eta", col="red", pch=16, ylim=c(1,100))
abline(h = eta, col = "red", lty = "dotted",lwd=4)
#dev.off()
#plot(lambdaBetaRVar[RInit:100,], main="Variance des estimation de Eta en fonction de r",xlab="R", ylab="Var(Eta)", col="red", pch=16)
#Graphes de Beta
#pdf("BetaFonctionDeRI.pdf", height=5,width=10)
#plot(All[10:97,2]~c(10:97), main="Estimation de Beta en fonction de r",xlab="R", ylab="Beta", col="red", pch=16)
#abline(h = beta, col = "blue", lty="dotted", lwd=4)
#plot(lambdaBetaRVar[RInit:100,-2], main="Variance de Beta en fonction de r", xlab="R", ylab="Var(Beta)", col="red",  pch=16)
#dev.off()

AllEta = matrix(0,99,8)
AllEta[15:25,1] = All1[15:25,1]
AllEta[25:35,2] = All2[25:35,1]
AllEta[35:45,3] = All3[35:45,1]
AllEta[45:55,4] = All4[45:55,1]
AllEta[55:65,5] = All5[55:65,1]
AllEta[65:75,6] = All6[65:75,1]
AllEta[75:85,7] = All7[75:85,1]
AllEta[85:95,8] = All8[85:95,1]

matplot(AllEta, xlab="R", ylab="Moyenne des Eta estimés",main = "Moyenne des estimations de Eta en fonction de R", pch = 16, lty = "dotted", lwd=4,ylim=c(60,110))
abline(h = eta, col = "red", lty = "dotted",lwd=4)
abline(v = 90, col = "red", lty = "dotted",lwd=4)
AllBeta = matrix(0,99,8)
AllBeta[15:25,1] = All1[15:25,2]
AllBeta[25:35,2] = All2[25:35,2]
AllBeta[35:45,3] = All3[35:45,2]
AllBeta[45:55,4] = All4[45:55,2]
AllBeta[55:65,5] = All5[55:65,2]
AllBeta[65:75,6] = All6[65:75,2]
AllBeta[75:85,7] = All7[75:85,2]
AllBeta[85:95,8] = All8[85:95,2]
matplot(AllBeta, xlab="R", ylab="Moyenne des Beta estimés",main = "Moyenne des estimations de Beta en fonction de R", pch = 16, lty = "dotted", lwd=4)
abline(h = beta, col = "blue", lty="dotted", lwd=4)
abline(v = 90, col = "blue", lty="dotted", lwd=4)
