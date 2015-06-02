## Simulation d'un echantillion censuré avec la loi Binomiale
n = 10
R = 6
sim = rexp(n,1)
sim = sim[order(sim)]
Moy = (n*sim[R]+R*sim[n])/(2*n)
p = Moy/sim[10]
tc = rbinom(10,floor(sim[10])+1,1-p)
