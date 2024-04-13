pkgload::load_all()
roxygen2::roxygenise()

data(SEAex)
dai<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam,avgRegion)])
print(dai)

dai["Vietnam:31",]
qpbox(dai,marginal ="Vietnam:31")

dai["Vietnam:31", "avgRegion:26"]
#Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y <= y)
dai["Malaysia:33 & Vietnam:31", "avgRegion:26"]
#' #Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y = y)
dai["Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE]
#' # Joint distribution with values set on their respective mean value
dai["mean:c(Vietnam,Thailand)",lower.tail=T]
colMeans(dai@data)
dai["Vietnam:31.63934 & Thailand:35.10656",lower.tail=T]
dai["Vietnam:31 & Thailand:35",lower.tail=T]
#' # Joint distribution with values set on their respective median value
dai["median:c(Vietnam, Thailand)",lower.tail=T]
#' # Joint distribution with xxxx
dai["Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=T]
#' # Condtional distribtuion distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
dai["Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)", fixed=TRUE]

dai["Malaysia:33 & median:c(Vietnam,Thailand)", "avgRegion:27", fixed=TRUE]

lapply(paste0("avgRegion:",c(21:35)),function(x) {
  print(x)
  dai["Malaysia:33 & median:c(Vietnam,Thailand)", x, fixed=TRUE]
  })

iop<-make_pbox(dai,pbx@copula)

showMethods(class="pbox")
help(package = pbox)
