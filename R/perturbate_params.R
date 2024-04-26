#' Perturb Parameters
#'
#' Method to perturb parameters of copula distributions.
#'
#' @name perturbate_params
#' @docType methods
#' @export
#'
#' @param paramMargins A list of parameter values for each distribution in the copula.
#'
#' @return A list of perturbed parameter values.
#'
#' @examples
#' paramMargins <- list(list(0.2, 0.3), list(0.4, 0.5))
#' perturbate_params(paramMargins)
#'
#' @importFrom stats rbinom rnorm

setGeneric("perturbate_params",
           def = function(paramMargins) {
             standardGeneric("perturbate_params")
           })

setMethod("perturbate_params",
          definition=function(paramMargins) {
  # Define a function to perturb a single parameter value
  perturb_param <- function(orig_param) {
    ind <- rbinom(1, 1, 0.5) == 1
    orig_param[ind] <- orig_param[ind] + rnorm(1, 0, 0.05)
    return(orig_param)
  }

  # Apply perturbation to each parameter in each distribution
  perturbed_params <- lapply(paramMargins, function(dist_params) {
    lapply(dist_params, function(param) {
      perturb_param(param)
    })
  })
  return(perturbed_params)
})

