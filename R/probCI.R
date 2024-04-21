#' Probability Confidence Interval
#'
#' Method to calculate the confidence interval around probabilities.
#'
#' @name probCI
#' @docType methods
#' @export
#'
#' @param probabilities A numeric vector of probabilities.
#' @param alpha The significance level for the confidence interval. Default is 0.05.
#'
#' @return A numeric vector containing the lower and upper bounds of the confidence interval.
#'
#' @examples
#' probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' probCI(probabilities)
#' probCI(probabilities, alpha = 0.1)
#'
#' @importFrom stats quantile

setGeneric("probCI",
           def = function(probabilities, alpha = 0.05) {
             standardGeneric("probCI")
           })

setMethod("probCI",
          definition=function(probabilities, alpha = 0.05) {
  # Calculate the confidence interval around the probabilities
  lower <- quantile(probabilities, alpha / 2)
  upper <- quantile(probabilities, 1 - alpha / 2)
  c(lower, upper)
})
