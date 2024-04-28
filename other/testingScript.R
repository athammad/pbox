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


roxygen2::roxygenise()
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





dai["Vietnam:31", "avgRegion:26"]
#Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y <= y)
dai["Malaysia:33 & Vietnam:31", "avgRegion:26"]
#Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y = y)
dai["Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE]
# Joint distribution with values set on their respective mean value
dai["mean:c(Vietnam,Thailand)",lower.tail=T]
colMeans(dai@data)
dai["Vietnam:31.63934 & Thailand:35.10656",lower.tail=T]
dai["Vietnam:31 & Thailand:35",lower.tail=T]
# Joint distribution with values set on their respective median value
dai["median:c(Vietnam, Thailand)",lower.tail=T]
# Joint distribution with xxxx
dai["Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=T]
# Condtional distribtuion distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
dai["Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)", fixed=TRUE]


qpbox(dai,marginal = "Vietnam:31 & avgRegion:26")
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",CI=T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32")
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",CI=T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32", fixed = T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",CI=T, fixed = T)



dai["Malaysia:33 & median:c(Vietnam,Thailand)", "avgRegion:27", fixed=TRUE]
qpbox(dai,marginal ="Malaysia:33 & median:c(Vietnam,Thailand)",conditional ="avgRegion:27",fixed = T)
lapply(paste0("avgRegion:",c(21:35)),function(x) {
  print(x)
  #dai["Malaysia:33 & median:c(Vietnam,Thailand)", x, fixed=TRUE]
  qpbox(dai,marginal ="Malaysia:33 & median:c(Vietnam,Thailand)",conditional =x,fixed = T)

  })


lapply(paste0("avgRegion:",c(21:35)),function(x) {
  print(x)
  #dai["Malaysia:33 & median:c(Vietnam,Thailand)", x, fixed=TRUE]
  dai@data[avgRegion==x]
#  qpbox(dai,marginal ="Malaysia:33 & median:c(Vietnam,Thailand)",conditional =x,fixed = T)

})

lapply(paste0("avgRegion:",c(23:26)),function(x) {
  print(x)
  #dai["Malaysia:33 & median:c(Vietnam,Thailand)", x, fixed=TRUE]
  qpbox(dai,marginal =x)
  #  qpbox(dai,marginal ="Malaysia:33 & median:c(Vietnam,Thailand)",conditional =x,fixed = T)

})

qpbox(dai,marginal ="avgRegion:35")

qpbox(dai,marginal ="avgRegion:35")
qpbox(dai,marginal ="Malaysia:33 & median:c(Vietnam,Thailand)",conditional =x,fixed = T)

bootstrap <- boot::boot(iris, corr.fun, R = 1000)
pMvdc(c(Inf,Inf,Inf,25),dai@copula)
rMvdc(100,dai@copula)
rCopula(100,dai@copula@copula)

gamlss::fitDist()


dai<-fread("./data/seaTS.csv")
pbx<-set_pbox(dai[CRN=="Indonesia",.(Rain, Temp,Hum, VHI)])

pbx["Temp:30 & VHI:70"]




############################################################################################

perturbate_params <- function(paramMargins) {
  # Define a function to perturb a single parameter value
  perturb_param <- function(orig_param) {
    ind = rbinom(1, 1, 0.5) == 1
    orig_param[ind] = orig_param[ind] + rnorm(1, 0, 0.05)
    return(orig_param)
  }

  # Apply perturbation to each parameter in each distribution
  perturbed_params <- lapply(paramMargins, function(dist_params) {
    lapply(dist_params, function(param) {
      perturb_param(param)
    })
  })
  return(perturbed_params)
}

probCI <- function(probabilities, alpha = 0.05) {
  # Calculate the confidence interval around the probabilities
  lower <- quantile(probabilities, alpha / 2)
  upper <- quantile(probabilities, 1 - alpha / 2)
  c(lower, upper)
}



perProb<-function(bpx,vecQuery){
  perCop<-copy(bpx)
  perCop@copula@paramMargins<-perturbate_params(bpx@copula@paramMargins)
  pMvdc(vecQuery,perCop@copula)
}

probCI(replicate(1000, perProb(dai,c(Inf,Inf,31,26))))
dai["Vietnam:31 & avgRegion:26", ]



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
qpbox(pbx,marginal="Malaysia:33")
#Get Joint distribution
qpbox(pbx,marginal="Malaysia:33 & Vietnam:34")
#Get Joint distribution
qpbox(pbx,marginal="Vietnam:31", conditional="avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y <= y)
qpbox(pbx,marginal="Malaysia:33 & Vietnam:31", conditional="avgRegion:26")
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


suppressWarnings(gamlss::fitDist(SEAex$Malaysia))
