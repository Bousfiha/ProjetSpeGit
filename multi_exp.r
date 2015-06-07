##Multi-censure pour la loi exponentielle

TailleEch = 100
lambda = 2
##Données observées
R= 50

delta=c(1:TailleEch)
y=c(1:TailleEch)
t=c(1:TailleEch)

exp = rexp(TailleEch,lambda)

for ( i in 1:TailleEch ) {
  y[i] = min(exp[i],t[i])
  
  if ( exp[i] <= t[i])
    delta[i] =1
  else
    delta[i] =0 
}

LambdaChap = sum(delta)/sum(y)