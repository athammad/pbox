
##### Marginal Distributions Fit  #######

SEAex[,lapply(.SD,fitDist)]
allDitrs<-lapply(SEAex,fitDist)

allDitrs$Malaysia$fits
'Table of AIC values'
do.call(cbind,map_depth(allDitrs,1,"fits"))

#Selected Distributions
map_depth(allDitrs,1,"family")
map_depth(allDitrs,1,"Allpar")


##### Copula Fit  #######

u <- pobs(SEAex)
#Fit copula
thetaVal <- 2 
copula <- evCopula(family = 'gumbel',param=thetaVal,dim=ncol(SEAex))
copula@parameters
fit.ml <- fitCopula(copula, u, method="ml")
#optimal val of Theta
coef(fit.ml)
summary(fit.ml)
copula <- evCopula(family = 'gumbel',param=coef(fit.ml),dim=ncol(DT_copula))

#goodness of fit
gf <- gofCopula(copula, x=as.matrix(DT_copula))
#gf
distList<-c(rep("evd",ncol(DT_copula)-1),"SHASHo2")
copSEA <- mvdc(copula, distList,
               unname(allDistrs))