## Simulation d'un echantillion censurÃ© de type III (Multicensuré)
n = 100
lambda = 0.1
Echs = matrix(0,90,2)
R = 0
for (k in 1:90){
  sim = rexp(n,1)
  ech = sim
  Tc = rexp(n,0.1*k)
  delta = c(1:n)
  for (i in 1:n) {
    if (Tc[i]<sim[i]) {
      delta[i] = 0
      ech[i] = Tc[i]
    } else {
      delta[i] = 1
    }   
  }
  R = sum(delta)
  lbdChap = R/sum(ech)
  Echs[k,1] = R
  Echs[k,2] = lbdChap
}
pdf("LambdaFoncRIII.pdf", height=5,width=10)
plot(Echs,col="blue", main="Estimation de Lambda en fonction de R",xlab="R" ,ylab="Lambda Estimé" ,pch=16)
abline(h = 1, col = "red", lwd=4)
dev.off()