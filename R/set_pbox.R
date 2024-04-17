##############################################################
#' Create a probability box from data.
#'
#' Method to automatically find the best marginal distribution and copula for a give dataset.
#'
#' @docType methods
#' @name set_pbox
#' @docType methods
#' @export
#' @import data.table
#' @include pbox.R
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param ... Other arguments to be passed to \code{fitDist}.
#' @return an object of class \code{pbox} with 3 slots:
#'
#' \code{data}: The original data coerced to a data.table.
#'
#' \code{copula}: The copula object of class \code{mvdc}.
#'
#' \code{fit}: The results of the automated selection for both the marginal distribution and the copula.
#'
#' @examples
#' data("SEAex")
#' pbx<- set_pbox(data=SEAex)
#' pbx
#' class(pbx)


setGeneric("set_pbox",
           def = function(data, ...) {
  standardGeneric("set_pbox")
})

setMethod("set_pbox",
          definition = function(data,...) {

  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }
  data.table::setDT(data)
  distSearch<-fit_dist_pbox(data)
  CopulaSearch<-fit_copula_pbox(data,.copula_families)

  finalCopula<-final_pbox(CopulaSearch,distSearch$allDitrs,data)
  cat("pbox object genrated!")

  obj <- new("pbox", data =data, copula=finalCopula,fit=list(distSearch,CopulaSearch))

})

