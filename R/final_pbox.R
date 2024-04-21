##############################################################
#' Fit the \code{mvdc} object.
#'
#' Combine all the results from \code{fit_copula_pbox} and \code{fit_dist_pbox} to build a Multivariate Distributions from Copula.
#'
#' @name final_pbox
#' @docType methods
#' @export
#' @include pbox.R
#'
#'
#' @param results_df The data.table with the correspondent AIC and the parameter estimates of the evaluated copulas and families resulting from \code{fit_copula_pbox}.
#' @param allDitrs The list of the fitted distributions for each variable resulting from \code{fit_dist_pbox}.
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @return An object of class \code{mvdc}.
#'
#'
#'
#' @examples
#' data(SEAex)
#' copulaFits<- fit_copula_pbox(data=SEAex,.copula_families)
#' distFits<- fit_dist_pbox(data=SEAex)
#'
#' final_pbox(copulaFits,distFits$allDitrs,SEAex)
#'
#' @importFrom utils getFromNamespace
#' @importFrom purrr map_depth map
#' @importFrom copula mvdc


setGeneric("final_pbox",
           def = function(results_df,allDitrs,data) {
             standardGeneric("final_pbox")
           })

setMethod("final_pbox",
          definition=function(results_df,allDitrs,data){

  bestCopula<-results_df[which.min(results_df$AIC),]
  copFun <- utils::getFromNamespace(bestCopula$copula,ns = "copula")
  cop <- copFun(family = bestCopula$family, param = bestCopula$coef, dim = ncol(data))

  distList<-unlist(unname(purrr::map(purrr::map_depth(allDitrs,1,"family"),1)))

  allPar <- unname(purrr::map_depth(allDitrs,1,coefAll2))
  # Function to modify the structure of each element in the list
  #modify_structure <- function(x) {names(x)<-gsub("eta.","",names(x));as.list(x)}
  # Applying the modification to each element of the list
  #allPar <- modify_depth(unname(map_depth(allDitrs,1,"Allpar")), 1, modify_structure)

  finalCop <- copula::mvdc(cop, distList,allPar)

  cat("---Final fitted copula---\n")
  cat("Copula Type:",bestCopula$copula,"\n")
  cat("Family:",bestCopula$family,"\n")
  cat("parameter:",bestCopula$coef,"\n")
  cat("--------------------\n")
  return(finalCop)
})




