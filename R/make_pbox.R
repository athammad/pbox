##############################################################
#' Make a pbox from a data.frame and a copula object.
#'
#' Ausiliary method to create a pbox object from a dataframe and a custom copula object.
#'
#' @name make_pbox
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param copula an object of class mvdc.
#' @return an object of class \code{pbox} with 2 slots:
#'
#' \code{data}: The original data coerced to a data.table.
#'
#' \code{copula}: The copula object of class \code{mvdc}.
#'

#' @examples
#' data("SEAex")
#'
#' pbx<- set_pbox(data=SEAex)
#' pbx
#' class(pbx)
#'

setGeneric("make_pbox",
           def = function(data,copula) {
             standardGeneric("make_pbox")
           })

setMethod("make_pbox",
          definition =  function(data,copula){
  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }
  if (!inherits(copula, c("mvdc"))) {
    stop("Input must be an object of class mvdc genrated using the copula package")
  }

  # ADD checks for colnames, number of marginal distributions
  if (!ncol(pbx@data)==pbx@copula@copula@dimension) {
    stop("The number of columns in the datset and the dimension of the copula object do not match")
  }
  data.table::setDT(data)

  obj <- new("pbox", data =data, copula=copula,fit=list())

})

