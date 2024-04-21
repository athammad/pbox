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
#' @param cop An object of class \code{mvdc}.
#'
#' @return An object of class \code{pbox} with 2 slots:
#'
#' \describe{
#'   \item{\code{data}}{The original data coerced to a data.table.}
#'   \item{\code{cop}}{The copula object of class \code{mvdc}.}
#' }
#'
#' @examples
#' library(copula)
#' data("SEAex")
#'
#' cop<-normalCopula(param = 0.5,dim = 4)
#' distList<-c("RG" , "SN1", "RG","RG" )
#' allDistrs<-list(list(mu = 31.07, sigma = 0.28),
#'                list(mu = 34.4, sigma = 0.98, nu = 1.7),
#'                list(mu = 31.4, sigma = 0.34),
#'                list(mu = 25.6, sigma = 0.24))
#' copSEA <- mvdc(cop, distList,allDistrs)
#' pbx<-make_pbox(data=SEAex,cop=copSEA)
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
           def = function(data,cop) {
             standardGeneric("make_pbox")
           })

setMethod("make_pbox",
          definition =  function(data,cop){
            if (!inherits(data, c("data.frame","data.table"))) {
              stop("Input must be a data frame or a data.table")
            }
            if (!inherits(cop, c("mvdc"))) {
              stop("Input must be an object of class mvdc genrated using the copula package")
            }

            # ADD checks for colnames, number of marginal distributions
            if (!ncol(data)==cop@copula@dimension) {
              stop("The number of columns in the datset and the dimension of the copula object do not match")
            }
            setDT(data)

            obj <- new("pbox", data =data, copula=cop,fit=list())

          })

