##############################################################
#' Build a Multivariate Distribution from Copula
#'
#' Combines the results from `fit_copula_pbox` and `fit_dist_pbox` to build a multivariate distribution from copula,
#' selecting the best copula based on AIC and utilizing the best-fitted marginal distributions.
#'
#' @name final_pbox
#' @export
#' @param results_df A data.table with AIC and parameter estimates of evaluated copulas and families from `fit_copula_pbox`.
#' @param allDitrs A list containing fitted distributions for each variable from `fit_dist_pbox`.
#' @param data A data frame or data table; this will be coerced to a `data.table` internally.
#' @return An object of class `mvdc` representing the combined multivariate distribution.
#' @examples
#' \dontrun{
#'   data("SEAex")
#'   copulaFits <- fit_copula_pbox(data = SEAex, .copula_families)
#'   distFits <- fit_dist_pbox(data = SEAex)
#'   final_mvd <- final_pbox(copulaFits, distFits$allDitrs, SEAex)
#'   print(final_mvd)
#' }
#' @importFrom utils getFromNamespace
#' @importFrom purrr map_depth map
#' @importFrom copula mvdc

setGeneric("final_pbox",
           def = function(results_df, allDitrs, data) {
             standardGeneric("final_pbox")
           })

#' @rdname final_pbox
#' @description
#' Method to construct a `mvdc` object by combining best-fit copula and marginal distribution results.
#' The method uses the best copula model as determined by the lowest AIC and combines it with
#' marginal distributions fitted to each variable.

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

  cat("\n\n---Final fitted copula---\n")
  cat("Copula Type:",bestCopula$copula,"\n")
  cat("Family:",bestCopula$family,"\n")
  cat("parameter:",bestCopula$coef,"\n")
  cat("--------------------------\n")
  return(finalCop)
})




