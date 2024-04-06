
##### Marginal Distributions Fit  #######

fit_dist_pbox<-function(data,...){

  allDitrs<-lapply(data,function(x) fitDist(x,...))
  distTable<-data.table(do.call(cbind,map_depth(allDitrs,1,"fits")), keep.rownames="DIST")
  return(list(allDitrs=allDitrs,distTable=distTable))
}

##### Copula Fit  #######
# Define the copula families and their corresponding parameters
copula_families <- list(
  archmCopula = c("clayton", "frank", "gumbel", "joe"),# "amh",
  evCopula = c("galambos", "gumbel", "huslerReiss" ),#"tawn" #"tev"
  ellipCopula = c("normal")# "t"
)

# Function to fit copula and return AIC value
fit_copula <- function(copula, family, dim, u) {
  if (dim > 2 && family %in% c("amh", "galambos", "huslerReiss", "tawn", "tev")) {
    return(NULL)  # Skip if the family is not available for dim > 2
  }
  copFun <- get(copula)
  cop <- copFun(family = family, param = NA_real_, dim = dim)


  fit <- fitCopula(cop, u, method = "ml")
  aicVal <- AIC(fit)
  coefVal <- coef(fit)

  return(data.table(copula=copula,family = family, AIC = aicVal, coef = coefVal))
}


fitCopulaPbox<-function(data,copula_families){

  u <- pobs(data)
  dfCopula <- setNames(stack(copula_families), c('family','copula'))

  # Perform grid search
  results <- apply(dfCopula, 1, function(row) {
    fit_copula(copula=row["copula"], family=row["family"], dim = ncol(data), u)

  })
  # Convert results to data frame
  results_df <- do.call(rbind, results)
  return(results_df)
}


#Fit togheter
finalFit<-function(results_df,allDitrs,data){

  bestCopula<-results_df[which.min(results_df$AIC),]
  copFun <- get(bestCopula$copula)
  cop <- copFun(family = bestCopula$family, param = bestCopula$coef, dim = ncol(data))

  distList<-unlist(unname(map(map_depth(allDitrs,1,"family"),1)))
  allPar<-unname(map_depth(allDitrs,1,"Allpar"))
  finalCop <- mvdc(cop, distList,allPar)

  cat("---Final fitted copula---\n")
  cat("Copula Type:",bestCopula$copula,"\n")
  cat("Family:",bestCopula$family,"\n")
  cat("parameter:",bestCopula$coef,"\n")
  cat("--------------------\n")
  return(finalCop)
}

#should use the results from fitDistPbox,fitCopulaPbox
#finalFit(results_df,allDitrs,SEAex)
