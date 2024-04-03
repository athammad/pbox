pacman::p_load(data.table,ggplot2,extRemes,
               copula,stargazer,gamlss.dist,
               fitdistrplus,gamlss)
######################################################################################
#
#https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/crucy.2304181636.v4.07/countries/
#
RegionNames<-c("Indonesia","Malaysia","Vietnam", "Cambodia","Brunei","Singapore",
               "Laos","Thailand","Myanmar","Philippines") #"East_Timor"
DT_long<-fread("./data/extremeSEA.csv")

gevFuncs<-function(x){
  tempts<-x
  ## Fit GEV ##
  fitGev <- fevd(tempts,type="GEV",method = "MLE",time.units="years",
                 use.phi = F,units = "deg C",
                 initial=list(location=mean(tempts), scale=sd(tempts)))
  ## Stationarity test ##
  station=is.fixedfevd(fitGev)
  ## Parameters of GEV ##
  pars<-list('loc' = fitGev$results$par[1], 'scale' = fitGev$results$par[2],
             'shape' = fitGev$results$par[3])
  ## ConfInt on the Parameters of GEV ##
  CI<-ci(fitGev, type = "parameter")
  ## N Year Return Level ##
  RL<-return.level(fitGev,return.period = c(2, 20, 50,100), do.ci=TRUE)
  ## Year Return temperature values ##
  # increase the median temperature by 0.1 until +1 C for each country
  tempSec<-seq(0.1,1,0.1)+quantile(tempts,probs=0.05)
  returnYears<-round(1/pextRemes(fitGev, q = tempSec, lower.tail = FALSE),3)
  ###RETURN ALL OBJECTS ###
  list("Fit"=fitGev,"Pars"=pars,"CI"=CI,"ReturnL"=RL,
       "station"=station,'returnYears'=returnYears)
}

seaGev<-lapply(DT_long,gevFuncs)
names(seaGev)


## Fit distribution to AVG temp SEA
fitAvg<-fitDist(seaTemp,type="realline")
fitAvg$fits
model<-gamlssML(seaTemp,family="SHASHo2")
coefz<-data.table(model$mu,model$sigma,
                  model$nu,model$tau)
names(coefz)<-model$parameters
plot(density(rSHASHo2(length(seaTemp),mu=coefz$mu,sigma= coefz$sigma,
                      nu=coefz$nu, tau=coefz$tau)))

## All distribution Toghter
allDistrs<-map_depth(seaGev,1,"Pars")
allDistrs$avgSEA<-as.list(coefz)
## All data tohghter
DT_long$avgRegion<-seaTemp

###############################################################################
# Copula #
###############################################################################
#Find param of Copula
myvars<-c("Indonesia","Malaysia","Vietnam","Cambodia","Brunei","Singapore","Laos",
          "Thailand", "Myanmar" ,"Philippines","avgRegion")
DT_copula<-DT_long[,..myvars]
cor(DT_copula,method = "kendall")
u <- pobs(DT_copula)
#Fit copula
thetaVal <- 2
copula <- evCopula(family = 'gumbel',param=thetaVal,dim=ncol(DT_copula))
copula@parameters
fit.ml <- fitCopula(copula, u, method="ml")
#optimal val of Theta
coef(fit.ml)
summary(fit.ml)
copula <- evCopula(family = 'gumbel',param=coef(fit.ml),dim=ncol(DT_copula))

gf <- gofCopula(copula, x=as.matrix(DT_copula))
gf
distList<-c(rep("evd",ncol(DT_copula)-1),"SHASHo2")
copSEA <- mvdc(copula, distList,
               unname(allDistrs))
copSEA@paramMargins

################################################################################
#-|Calculate the marginal distribution of each one of them |-#
#-|The joint Probability of all of them |-#
#-|Joint Probability of the first two only|-#
#-|The conditional probability of first|Second |-#
################################################################################

####  1) Marginal Probability ####
# You can get the marginal probability of any variable by setting Inf
#pSHASHo2(mean(DT_copula$avgRegion),mu=coefz$mu,sigma= coefz$sigma, nu=coefz$nu, tau=coefz$tau)
# Same
pMvdc(c(rep(Inf,ncol(DT_copula)-1),mean(DT_copula$avgRegion)),copSEA)

####  2) Joint Probability All ####
pMvdc(as.numeric(DT_copula[,lapply(.SD,median)]),copSEA)

####  3) Joint Probability of the first two only ####
pMvdc(c(mean(DT_copula$Indonesia), mean(DT_copula$Malaysia),Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf),copSEA)
pMvdc(c(Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,mean(DT_copula$Philippines),mean(DT_copula$avgRegion)),copSEA)

####  4) The conditional probability of first|Second ####
# Pr(X <= 23 | Y <= 21)

pMvdc(c(31, 31, Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf), copSEA) / pMvdc(c(Inf, 31, Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf), copSEA)


####
setProbBox<-function(
    ### TO DO
  #Allows to define the Marginal Distribution and copula Structure
  #If no definition is provided, fit best margins and copula based on BIC/AIC
  # Store results as metadata of the table
  # return an object of class "probTab"
)

  pq<-function(data=data,cop=cop, query=""){

    ## Basic Function to query a "Probabilistic dataFrame".
    #+ Probabilistic DataFrame = DataFrame + Copula
    #+

    varnames<-names(data)
    placeholder<-rep(Inf,ncol(data))
    varSet<-cbind.data.frame(varnames,placeholder)

    if (grepl("|", query, fixed=TRUE)) {
      split_queries <- strsplit(query, "\\|")[[1]]

      cond<-lapply(split_queries,function(x){
        matches <-as.data.frame(stringr::str_match_all(x, "(\\w+)=(\\d+)")[[1]])[,c(2,3)]
        names(matches)<-names(varSet)
        matches$placeholder<-as.numeric(matches$placeholder)
        setDT(varSet);setDT(matches)
        # Replace values in the dataframe
        varSet[match(matches$varnames, varSet$varnames),]<-matches
        #query copula
        p=pMvdc(varSet$placeholder, copSEA)
        p
      })
      print(paste("The Conditional Probability of",query, "is", cond[[1]]/cond[[2]]))
    }else {

      matches <-as.data.frame(stringr::str_match_all(query, "(\\w+)=(\\d+)")[[1]])[,c(2,3)]
      names(matches)<-names(varSet)
      matches$placeholder<-as.numeric(matches$placeholder)
      setDT(varSet);setDT(matches)
      # Replace values in the dataframe
      varSet[match(matches$varnames, varSet$varnames),]<-matches
      #query copula
      print(paste("The Probability of",query, "is",pMvdc(varSet$placeholder, cop)))
    }
  }

#Joint
pq(data=DT_copula,cop=copSEA,query = "Indonesia=33 & Vietnam=34")
pq(data=DT_copula,cop=copSEA,query = "Indonesia=33 & Vietnam=34 & Laos=31")

#Marginal
pq(data=DT_copula,cop=copSEA,query = "Indonesia=32")
pq(data=DT_copula,cop=copSEA,query = "Laos=33")

#The conditional probability of first|Second Pr(X <= x | Y <= x)
pq(data=DT_copula,cop=copSEA,query = "Vietnam=31| avgRegion=26")
pq(data=DT_copula,cop=copSEA,query = "Indonesia=32 & Vietnam=34| avgRegion=26")

#The conditional probability of first|Second Pr(X <= x | Y = y)
#Y is equal to a specific value
#xxxxxxx

# Manual Testing
pMvdc(c(32, Inf, 34 ,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf), copSEA)/pMvdc(c(Inf, Inf, Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,26), copSEA)


#### DREAM Probabilistic Data Table
PDT<-pfread("climate_data.csv")
PDT<-pset(PDT)# stima best distrib for each col and best underlying copula structure
# Now can query the PDT
PDT[Indonesia=32 & Vietnam=34]
PDT[Indonesia=32 & Vietnam=34,lower.tail = TRUE]
PDT[Indonesia=32 & Vietnam=34| avgRegion=26]
PDT[Indonesia=32 & Laos=mean]
PDT[Indonesia=32 | Laos=mean]
PDT[Indonesia=c(29:32) | Vietnam=34]
pnorm()
