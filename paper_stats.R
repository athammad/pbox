pacman::p_load(data.table,ggplot2,MASS,extRemes,
               copula,stargazer,
               lubridate,jtools,gamlss.dist,
               fitdistrplus,gamlss,purrr)

source("helperFuncs.R")
SEAex<-fread("./Data/SEAex.csv")

### Fit distribution to extreme temp Malaysia,Thailand,Vietnam
seaGev<-lapply(SEAex[,.(Malaysia,Thailand,Vietnam)],gevFuncs)
names(seaGev)

## Fit distribution to avgRegion temp SEA
fitAvg<-fitDist(SEAex$avgRegion,type="realline")
fitAvg$fits
model<-gamlssML(SEAex$avgRegion,family="SHASHo2")
coefz<-data.table(model$mu,model$sigma,
                  model$nu,model$tau)
names(coefz)<-model$parameters
plot(density(rSHASHo2(length(SEAex$avgRegion),mu=coefz$mu,sigma= coefz$sigma, 
                      nu=coefz$nu, tau=coefz$tau)))


plot.ts(SEAex,plot.type = "single")
cor(SEAex,method = "kendall")


# Copula object 
u <- pobs(SEAex)
#Fit copula
thetaVal <- 2 
copula <- evCopula(family = 'gumbel',param=thetaVal,dim=ncol(SEAex))
copula@parameters
fit.ml <- fitCopula(copula, u, method="ml")
#optimal val of Theta
coef(fit.ml)
summary(fit.ml)
copula <- evCopula(family = 'gumbel',param=coef(fit.ml),dim=ncol(SEAex))

#gf <- gofCopula(copula, x=as.matrix(SEAex))
#gf
allDistrs<-map_depth(seaGev,1,"Pars")
allDistrs$avgSEA<-as.list(coefz)
distList<-c(rep("evd",ncol(SEAex)-1),"SHASHo2")
copSEA <- mvdc(copula, distList,
               unname(allDistrs))
copSEA@paramMargins
class(copSEA)
####
#Marginal
#PDT[Indonesia<32]
pq(data=SEAex,cop=copSEA,query = "Indonesia<32")
pq(data=SEAex,cop=copSEA,query = "Laos<33")


#Joint
pq(data=SEAex,cop=copSEA,query = "Indonesia<33 & Vietnam<34",lower.tail = F)
pq(data=SEAex,cop=copSEA,query = "Indonesia<33 & Vietnam<34 & Laos<31",lower.tail = F)

#The conditional probability of first|Second Pr(X <= x | Y <= x)
pq(data=SEAex,cop=copSEA,query = "Vietnam<31| avgRegion<26",lower.tail = F)
pq(data=SEAex,cop=copSEA,query = "Indonesia<32 & Vietnam<34| avgRegion<26")

#The conditional probability of first|Second Pr(X <= x | Y = y)
#Y is equal to a specific value
pqFix(data=SEAex,cop=copSEA,query = "Vietnam<31 & Indonesia<31|avgRegion=26")
pqFix(data=SEAex,cop=copSEA,query = "Vietnam<31|avgRegion=26")
pqFix(data=SEAex,cop=copSEA,query = "Vietnam>31|avgRegion=25",lower.tail = F)


################################################################################

dfGrid<-expand.grid(A=quantile(SEAex$Malaysia,seq(0, 1, 0.5)),
                    B=quantile(SEAex$Vietnam,seq(0, 1, 0.5)),
                    C=quantile(SEAex$avgRegion,seq(0, 1, 0.5)))
dfGrid$block<-rep(1:3,3)
dai<-function(x){
  pqFix(data=SEAex,cop=copSEA,query = paste0("Malaysia>",x["A"]," & Vietnam>",x["B"],"|avgRegion=",x["C"]),lower.tail = F)
  
}

dfGrid$results <- apply(dfGrid, 1, dai)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Create a new PBT object
SEAex<-fread("./Data/SEAex.csv")
pbx<- setPBX(data=SEAex)
pbx
class(pbx)
typeof(pbx)
class(pbx@data)
class(pbx@copula)
typeof(pbx@data)
typeof(pbx@copula)

pbx["Malaysia:33 & Vietnam:34",]
pq(data=SEAex,cop=copSEA,query = "Malaysia<33 & Vietnam<34",lower.tail = T)

pbx[]
pbx["Vietnam:31", "avgRegion:26",lower.tail=F]
pq(data=SEAex,cop=copSEA,query = "Vietnam<31| avgRegion<26",lower.tail = F)


pbx["Malaysia:33 & Vietnam:31", "avgRegion:26",lower.tail=F]
pq(data=SEAex,cop=copSEA,query = "Malaysia<33 & Vietnam<31| avgRegion<26",lower.tail = F)

pbx["Malaysia:33 & Vietnam:31", "avgRegion:26",lower.tail=F,fixed=TRUE]
pqFix(data=SEAex,cop=copSEA,query ="Malaysia>33 & Vietnam>31 | avgRegion>26",lower.tail = F)

pbx["mean:c(Vietnam,Thailand)",lower.tail=T]
colMeans(SEAex[,.(Vietnam, Thailand)])
pMvdc(c(Inf,35.10656,31.63934,Inf), copSEA)

pbx["median:c(Vietnam, Thailand)",lower.tail=T]
apply(SEAex[,.(Vietnam, Thailand)],2,median)
pMvdc(c(Inf,35.1,31.6,Inf), copSEA)

pbx["Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=T]
pMvdc(c(33,35.10656,31.63934,Inf), copSEA)

pbx["Malaysia:33 & mean:c(Vietnam, Thailand)", "avgRegion:26",lower.tail=T]
pMvdc(c(33,35.10656,31.63934,Inf), copSEA)/pMvdc(c(Inf,Inf,Inf,26), copSEA)

pbx["Malaysia:33 & median:c(Vietnam, Thailand)", "avgRegion:26",lower.tail=T]
pMvdc(c(33,35.1,31.6,Inf), copSEA)/pMvdc(c(Inf,Inf,Inf,26), copSEA)

pbx["Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)",lower.tail=T]
pMvdc(c(33,35.1,31.6,Inf), copSEA)/pMvdc(c(Inf,Inf,Inf,25.78951), copSEA)

######TO DO
#+ aoutomate and create table for setPBX
#+ comment all the functions
#+ print method for pbox object DONE
#+ keep c() even when one single variable? YES




# # Extract operators, countries, and values from the string using regular expressions
# matches <- as.data.frame(stringr::str_match_all("Malaysia:33 & avgRegion:26 & mean:c(Thailand) & median:c(Vietnam)", "([a-zA-Z]+)(:)(\\d+)|(\\w+)([:])c\\((.+)\\)")[[1]])[, -1]
# colnames(matches) <- c("Operator1", "Colon1", "Value1", "Operator2", "Colon2", "Countries")
# matches

library(stringr)

# Define the string
input_string <- "Malaysia:33 & avgRegion:26 & mean:c(Thailand) & median:c(Vietnam)"
input_string <- "Malaysia:33"
input_string <- "Malaysia:33 & avgRegion:26"
input_string <- "Malaysia:33 & mean:c(Thailand,Vietnam)"
input_string <- "mean:c(Thailand) & median:c(Vietnam)"
input_string <- "mean:c(Thailand,Vietnam)"

# Define the regular expression pattern
pattern <- "([a-zA-Z]+)(:)(\\d+)|(\\w+)([:])c\\(([^)]+)\\)"

# Extract matches using the regular expression pattern
matches <- as.data.frame(stringr::str_match_all(input_string, pattern)[[1]])[, -1]
colnames(matches) <- c("Operator1", "Colon1", "Value1", "Operator2", "Colon2", "Countries")
matches


#dt["Malaysia:33 & quantile:c(Vietnam=0.5 & Thailand=0.2)", "avgRegion:26",lower.tail=F] too complicate for now! later in V2


pb["Malysia:33 & "]
gsub("[[:blank:]]", "", "Malaysia:33 & quantile:c(Vietnam=0.5 & Thailand=0.2)")


