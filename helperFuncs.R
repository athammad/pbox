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

pq<-function(data=data,cop=cop, query="",lower.tail=TRUE){
    
    ## Basic Function to query a "Probabilistic dataFrame".
    #+ Probabilistic DataFrame = DataFrame + Copula 
    #+ 
    
    varnames<-names(data)
    placeholder<-rep(Inf,ncol(data))
    varSet<-cbind.data.frame(varnames,placeholder)
    
    if (grepl("|", query, fixed=TRUE)) {
      split_queries <- strsplit(query, "\\|")[[1]]
      
      cond<-lapply(split_queries,function(x){
        matches <-as.data.frame(stringr::str_match_all(x, "(\\w+)([=|<|>])(\\d+)")[[1]])[,c(2,4)]
        names(matches)<-names(varSet)
        matches$placeholder<-as.numeric(matches$placeholder)
        setDT(varSet);setDT(matches)
        # Replace values in the dataframe
        varSet[match(matches$varnames, varSet$varnames),]<-matches
        #query copula
        p=pMvdc(varSet$placeholder, cop)
        p
      })
      probres<-cond[[1]]/cond[[2]]
      if(lower.tail==FALSE){probres<-1-probres}
      print(paste("The Conditional Probability of",query, "is",probres))
    }else {
      
      matches <-as.data.frame(stringr::str_match_all(query, "(\\w+)([=|<|>])(\\d+)")[[1]])[,c(2,4)]
      names(matches)<-names(varSet)
      matches$placeholder<-as.numeric(matches$placeholder)
      setDT(varSet);setDT(matches)
      # Replace values in the dataframe
      varSet[match(matches$varnames, varSet$varnames),]<-matches
      probres<-pMvdc(varSet$placeholder, cop)
      if(lower.tail==FALSE){probres<-1-probres}
      print(paste("The Probability of",query, "is",probres))
      
    }
  }

#Joint
# pq(data=DT_copula,cop=copSEA,query = "Indonesia<33 & Vietnam<34",lower.tail = F)
# pq(data=DT_copula,cop=copSEA,query = "Indonesia<33 & Vietnam<34 & Laos<31",lower.tail = F)
# 
# #Marginal
# pq(data=DT_copula,cop=copSEA,query = "Indonesia<32")
# pq(data=DT_copula,cop=copSEA,query = "Laos<33")
# 
# #The conditional probability of first|Second Pr(X <= x | Y <= x)
# pq(data=DT_copula,cop=copSEA,query = "Vietnam<31| avgRegion<26",lower.tail = F)
# pq(data=DT_copula,cop=copSEA,query = "Indonesia<32 & Vietnam<34| avgRegion<26")



pqFix<-function(data=data,cop=cop, query="",lower.tail=TRUE){
  varnames<-names(data)
  placeholder<-rep(Inf,ncol(data))
  varSet<-cbind.data.frame(varnames,placeholder)
  
  split_queries <- strsplit(query, "\\|")[[1]]
  
  condFix<-lapply(split_queries,function(x){
    matches <-as.data.frame(stringr::str_match_all(x, "(\\w+)([=|<|>])(\\d+)")[[1]])[,c(2,4)]
    names(matches)<-names(varSet)
    matches$placeholder<-as.numeric(matches$placeholder)
    setDT(varSet);setDT(matches)
    # Replace values in the dataframe
    varSet[match(matches$varnames, varSet$varnames),]<-matches
    #query copula
    p=pMvdc(varSet$placeholder, cop)
    p
  })
  condFix<-cCopula(
    cbind( condFix[[1]],  condFix[[2]]), 
    indices = 2, 
    copula = cop@copula
  )
  if(lower.tail==FALSE){condFix<-1-condFix}
  print(paste("The Conditional Probability of",query, "is", condFix))
  return(condFix)
}

#The conditional probability of first|Second Pr(X <= x | Y = y)
#Y is equal to a specific value
#pqFix(data=DT_copula,cop=copSEA,query = "Vietnam<31 & Indonesia<31|avgRegion=26")


#xxxxxxx
# Pr(X <= x | Y = y)
# cCopula(
#   cbind(pMvdc(c(31, Inf, 31 ,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf), copSEA),
#         pMvdc(c(Inf, Inf, Inf ,Inf,Inf,Inf,Inf,Inf,Inf,Inf,26), copSEA)), 
#   indices = 2, 
#   copula = copSEA@copula
# )

# Manual Testing
# pMvdc(c(34, Inf, 31 ,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf), copSEA)/pMvdc(c(Inf, Inf, Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,25), copSEA) 
# colMeans(DT_copula)


#### DREAM Probabilistic Data Table
# PDT<-pfread("climate_data.csv")
# PDT<-pset(PDT)# stima best distrib for each col and best underlying copula structure
# # Now can query the PDT
#PDT[Indonesia=32 & Vietnam=34]
# PDT[Indonesia=32 & Vietnam=34,lower.tail = TRUE]
# PDT[Indonesia=32 & Vietnam=34| avgRegion=26]
# PDT[Indonesia=32 & Laos=mean]
# PDT[Indonesia=32 | Laos=mean]
# PDT[Indonesia=c(29:32) | Vietnam=34]
# Versione Montecarlo per intervals!! jack


matcher<-function(i,varnames,placeholder,varSet,x){
  matches <-as.data.frame(stringr::str_match_all(i, "(\\w+)([=|<|>])(\\d+)")[[1]])[,c(2,4)]
  names(matches)<-names(varSet)
  matches$placeholder<-as.numeric(matches$placeholder)
  setDT(varSet);setDT(matches)
  # Replace values in the dataframe
  varSet[match(matches$varnames, varSet$varnames),]<-matches
  probres<-pMvdc(varSet$placeholder, x@copula)
  if(lower.tail==FALSE){probres<-1-probres}
  probes
}
