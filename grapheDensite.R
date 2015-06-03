#mat = matrix(0,1990,3)
#vec = seq(0,198.9,by=0.1)
#for (i in 1:1990) {
#  mat[i,1] = dweibull(vec[i],3,100)
#  mat[i,2] = dweibull(vec[i],3,50)
#  mat[i,3] = dweibull(vec[i],2,100)
#}
#pdf("Densite_Weibull.pdf", height=10,width=10)
#matplot(vec,mat,main="Densité de Probabilité - Weibull",xlab="",ylab="",type="l",lwd=6)
#legend("topright", legend = c("Eta = 100 Beta = 3", "Eta =   50 Beta = 3","Eta = 100 Beta = 2"), col = c("black", "red", "green"), pch = 15, bty = "n", pt.cex = 2, cex = 1.5, text.col = "forestgreen", inset = c(0.1, 0.1))
#dev.off()

mat = matrix(0,500,3)
vec = seq(0,49.9,by=0.1)
for (i in 1:500) {
  mat[i,1] = dexp(vec[i],0.5)
  mat[i,2] = dexp(vec[i],1)
  mat[i,3] = dexp(vec[i],2)
}
pdf("Densite_Exp.pdf", height=10,width=10)
matplot(vec,mat,main="Densité de Probabilité - Exponentielle",xlab="",ylab="",type="l",lwd=6)
legend("topright", legend = c("Lambda = 0.5", "Lambda = 1","Lambda = 2"), col = c("black", "red", "green"), pch = 15, bty = "n", pt.cex = 2, cex = 1.2, text.col = "forestgreen", inset = c(0.1, 0.1))
dev.off()