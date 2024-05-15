roxygen2::roxygenise()
devtools::test()
usethis::use_test()
usethis::use_vignette("pbox_vignette", "The pbox vignette")
#devtools::build()
pkgload::load_all()
prova<-devtools::check()
usethis::use_news_md()
commenti<-usethis::use_cran_comments()
#devtools::release()
libary(goodpractice)
goodpractice::gp()



#####TEST ALL
prova<-devtools::check()
pkgload::load_all()
data(SEAex)
dai<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam,avgRegion)])
print(dai)

qpbox(dai,marginal = "Vietnam:31 & avgRegion:26")
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",CI=T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32")
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",CI=T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32", fixed = T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",CI=T, fixed = T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",fixed = T)

library(goodpractice)
goodpractice::gp()



cop<-normalCopula(param = 0.5,dim = 4)
distList<-c("RG" , "SN1", "RG","RG" )
allDistrs<-list(list(mu = 31.07, sigma = 0.28),
                list(mu = 34.4, sigma = 0.98, nu = 1.7),
                list(mu = 31.4, sigma = 0.34),
                list(mu = 25.6, sigma = 0.24))

allDistrs<-list(A=list(mu = 31.07, sigma = 0.28),
     B=list(mu = 34.4, sigma = 0.98, nu = 1.7),
     C=list(mu = 31.4, sigma = 0.34),
     D=list(mu = 25.6, sigma = 0.24))
copSEA <- mvdc(cop, distList,
               allDistrs)
pbox::make_pbox(data=SEAex,copula=copSEA)

data(SEAex)
pbx<-set_pbox(SEAex[,.(Malaysia,Thailand)])


vecQuery <- c(31, 34)
perProb(pbx, vecQuery)


data("SEAex")
pbx<-set_pbox(SEAex)
#Get marginal distribution
qpbox(pbx,mj="Malaysia:33")
#Get Joint distribution
qpbox(pbx,mj="Malaysia:33 & Vietnam:34")
#Get Joint distribution
qpbox(pbx,mj="Vietnam:31", co="avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y <= y)
qpbox(pbx,mj="Malaysia:33 & Vietnam:31", co="avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE)
# Joint distribution with values set on their respective mean value
qpbox(pbx,"mean:c(Vietnam,Thailand)",lower.tail=TRUE)
# Joint distribution with values set on their respective median value
qpbox(pbx,"median:c(Vietnam, Thailand)",lower.tail=TRUE)
# Joint distribution with xxxx
qpbox(pbx,"Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=TRUE)
# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)")
# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)",CI=TRUE,iter=100)

###################################################################################
pkgload::load_all()
data("SEAex")
pbx<-set_pbox(SEAex)
pbx


usethis::use_version()


#Get marginal distribution
qpbox(pbx,mj = "Malaysia:33")

#Get Joint distribution
qpbox(pbx,mj = "Malaysia:33 & Vietnam:34")

# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,mj = "Malaysia:33 & median:c(Vietnam,Thailand)", co="mean:c(avgRegion)", fixed=TRUE)

# Estimate confidence intervals
qpbox(pbx,mj = "Vietnam:31 & avgRegion:26", co="Malaysia:32",CI=T)

grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))

seq(-3,3,1)
scenario_pbox(pbx,mj = "Vietnam:31 & avgRegion:26", param_list = list(Vietnam="mu"))

############################################################################
# TESTING FOLDER #
usethis::use_version()
pkgload::load_all()
library(usethis)
usethis::use_test("fit_dist_pbox.R")

use_test("qpbox")


set_pbox(SEAex)
