#' Compute evaluation metrics to measure regression performance.
#'
#' RMSE,MAPE and R2, are accompanied with additional metrics specifically designed to evaluate the goodness of the prediction intervals including
#' interval score,sharpness, underprediction, overprediction.
#' @param SCFobj Object of class syntCF.
#' @param quantiles Vector of quantiles for quantile prediction.
#' @param ... Other arguments to be passed to predict.
#' @return an object of class syntCF with the values for RMSPE, MAPE,R2 and the train and test set and vector of scoring rules metrics.
#' @examples
#'
#' \dontrun{
#' modelEval<-syntCFmetrics(trainModel,quantiles=c(0.1, 0.9))
#' modelEval$trainr$trainMetrics
#' modelEval$testr$testMetrics
#' modelEval$quants$intervalMetrics
#' }
#'
#' @export
#' @import data.table
#' @import caret
#' @import MLmetrics
#' @import scoringutils
#' @importFrom stats lm predict time
#' @importFrom utils askYesNo head tail


##### Marginal Distributions Fit  #######
pacman::p_load(data.table,ggplot2,MASS,extRemes,
               copula,stargazer,
               lubridate,jtools,gamlss.dist,
               fitdistrplus,gamlss,purrr)

#source("helperFuncs.R")
SEAex<-fread("./data/SEAex.csv")
SEAex<-SEAex[,.(Vietnam,avgRegion,Malaysia)]

fitDistPbox<-function(SEAex,...){

allDitrs<-lapply(SEAex,fitDist(...))
distTable<-data.table(do.call(cbind,map_depth(allDitrs,1,"fits")), keep.rownames="DIST")
#Selected Distributions
#map_depth(allDitrs,1,"family")
#map_depth(allDitrs,1,"Allpar")
return(list(allDitrs,distTable))
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
  print(row)
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
finalFit(results_df,allDitrs,dim=ncol(SEAex))

set_pbox <- function(data) {
  ############################################
  #+Questo dovrebbe sparare fuori una mega tabella
  #+con tutti i BIC/ AIC alla pycret
  ############################################
  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }
  setDT(data)
  distSearch<-fitDistPbox
  CopulaSearch<-fitCopulaPbox
  ##
  ##
  ##
  ##

  finalCopula<-finalFit(distSearch$allDitrs,CopulaSearch,dim=ncol(data))
  print("pbox object genrated!")

  obj <- new("pbox", data =data, copula=finalCopula,fit=list(distSearch,CopulaSearch))

}

