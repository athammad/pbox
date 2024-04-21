#' Extract Coefficients
#'
#' Internal method to extract coefficients from an object.
#'
#' @name coefAll2
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param obj An object.
#' @param deviance Logical value indicating whether to compute deviance.
#' @return A list of coefficients.
#'

setGeneric(".coefAll2",
           def = function(obj, deviance = FALSE, ...) {
             standardGeneric(".coefAll2")
           })

setMethod(".coefAll2",
          definition=function (obj, deviance = FALSE, ...){
  #fix to issue with sigma in function coefAll of gamlss
  out <- list()
  if ("mu" %in% obj$parameters)
    out$mu <- obj$mu
  if ("sigma" %in% obj$parameters)
    out$sigma <- obj$sigma
  if ("nu" %in% obj$parameters)
    out$nu <- obj$nu
  if ("tau" %in% obj$parameters)
    out$tau <- obj$tau
  if (deviance)
    out$deviance <- deviance(obj)
  return(out)
})
