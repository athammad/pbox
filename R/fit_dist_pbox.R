##############################################################
#' Marginal Distributions Fit
#'
#' Method to automatically find the best marginal distribution of each variable in a data.frame.
#'
#' @name fit_dist_pbox
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param ... Other arguments to be passed to \code{fitDist}.
#' @return a list containing 2 elements:
#'
#' \code{allDitrs}: List of the fitted distributions for each variable.
#'
#' \code{distTable}: \code{data.frame} with numeric value with the corresponding Akaike's Information Criterion \bold{AIC} for each distribution tested.
#'
#'
#' @examples
#' data(SEAex)
#' distFits<- fit_dist_pbox(data=SEAex)
#' distFits$allDitrs
#' distFits$distTable
#'

setGeneric("fit_dist_pbox",
           def = function(data,...) {
             standardGeneric("fit_dist_pbox")
           })

setMethod("fit_dist_pbox",
          definition=function(data,...){

  allDitrs<-suppressWarnings(lapply(data,function(x) gamlss::fitDist(x,...)))
  distTable<-data.table(do.call(cbind,purrr::map_depth(allDitrs,1,"fits")), keep.rownames="DIST")
  return(list(allDitrs=allDitrs,distTable=distTable))
})
