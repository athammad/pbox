##############################################################
#' Make a Pbox
#'
#' Auxiliary method to create a pbox object from a dataframe and a custom copula object.
#'
#' @name make_pbox
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param copula An object of class \code{mvdc}.
#'
#' @return An object of class \code{pbox} with 2 slots:
#'
#' \describe{
#'   \item{\code{data}}{The original data coerced to a data.table.}
#'   \item{\code{copula}}{The copula object of class \code{mvdc}.}
#' }
#'
#' @examples
#' data("SEAex")
#' pbx <- make_pbox(data = SEAex, copula = some_mvdc_object)
#' pbx
#' class(pbx)
#'
#' @importFrom data.table setDT
#'
#'
#'
#'
#'

globalVariables(c(".", "..varnames", "Operator","Value", "Varnames" ,"Varnames2" ,"pbx"))


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
            setDT(data)

            obj <- new("pbox", data =data, copula=copula,fit=list())

          })

