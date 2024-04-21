##############################################################
#' Copula Fit
#'
#' Method to automatically find the best Copula given a data.frame. Wrapper around the function \code{fitCopula}.
#'
#' @name fit_copula_pbox
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param .copula_families List of copula types  and their corresponding families Currently \bold{pbox} supports only Archimedean, Elliptical and Extreme-Value copula.
#' Currently supported families are "clayton", "frank", "amh", "gumbel", and "joe" for Archimedean Copula.
#' "galambos", "gumbel" and "huslerReiss" for Extreme-Value copula.
#' "normal" and "t" for Elliptical copula.
#'
#' @return a data.table with the correspondent AIC and the parameter estimates of the evaluated copulas and families.
#'
#'
#' @examples
#' data(SEAex)
#' # Define the copula families and their corresponding parameters
#'  .copula_families <- list(
#'  archmCopula = c("clayton", "frank", "gumbel", "joe"),# "amh",
#'  evCopula = c("galambos", "gumbel", "huslerReiss" ),#"tawn" #"tev"
#'  ellipCopula = c("normal")# "t"
#')
#' distFits<- fit_copula_pbox(data=SEAex,.copula_families)
#' distFits
#'
#'
#' @importFrom stats setNames
#' @importFrom utils stack


setGeneric("fit_copula_pbox",
           def = function(data,.copula_families) {
             standardGeneric("fit_copula_pbox")
           })

setMethod("fit_copula_pbox",
          definition=function(data,.copula_families){

  u <- copula::pobs(data)
  dfCopula <- stats::setNames(utils::stack(.copula_families), c('family','copula'))

  # Perform grid search
  results <- apply(dfCopula, 1, function(row) {
    .fit_copula(copula=row["copula"], family=row["family"], dim = ncol(data), u)

  })
  # Convert results to data frame
  results_df <- do.call(rbind, results)
  return(results_df)
})


