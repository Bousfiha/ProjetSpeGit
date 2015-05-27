##Quantiles pour la loi exponentielle

##Tableau de quantiles au seuil 5%
##en fonctiondu taux de censure
quantile_CM = c(1:7)
quantile_AD = c(1:7)
##Tableau du nombre de données censurées
R=c(1:7)
##Tableau des risques de première espèce
alpha_ad=c(1:7)
alpha_cm=c(1:7)
puissance_ad=c(1:7)
puissance_cm=c(1:7)


##p = 0.2
R[1]=0.2*TailleEch
quantile_CM[1] = 0.017
quantile_AD[1] = 0.252
##p=0.4
R[2]=0.4*TailleEch
quantile_CM[2] = 0.056
quantile_AD[2] = 0.497
##p=0.6
R[3]=0.6*TailleEch
quantile_CM[3] = 0.109
quantile_AD[3] = 0.733
##p=0.8
R[4]=0.8*TailleEch
quantile_CM[4] = 0.169
quantile_AD[4] = 0.993
##p=0.9
R[5]=0.9*TailleEch
quantile_CM[5] = 0.196
quantile_AD[5] = 1.135
##p=0.95
R[6]=0.95*TailleEch
quantile_CM[6] = 0.209
quantile_AD[6] = 1.212
##p=1
R[7]=1*TailleEch
quantile_CM[7] = 0.220
quantile_AD[7] = 1.308