#' Perturbed Probability
#'
#' Internal method to compute the probability using a perturbed copula.
#'
#' @name perProb
#' @docType methods
#' @export
#'
#' @param x An object of class \code{pbox} representing the base copula.
#' @param vecQuery A numeric vector representing the query values.
#'
#' @return The probability computed using a perturbed copula.
#'
#' @examples
#' data(SEAex)
#' pbx<-set_pbox(SEAex[,.(Malaysia,Thailand)])
#' vecQuery <- c(31, 34)
#' perProb(pbx, vecQuery)
#'
#' @importFrom data.table copy
#' @importFrom copula pMvdc
#' @include pbox.R


setGeneric("perProb",
           def = function(x,vecQuery) {
             standardGeneric("perProb")
           })

setMethod("perProb",
          signature = "pbox",
          definition=function(x,vecQuery){
            if (!inherits(x, c("pbox"))) {
              stop("Input must be a pbox object!")
            }
  perCop<-copy(x)
  oMargins<-x@copula@paramMargins
  perCop@copula@paramMargins<-perturbate_params(oMargins)
  pMvdc(vecQuery,perCop@copula)
})
