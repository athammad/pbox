#' Summary Statistics
#'
#' Internal method to compute summary statistics of a numeric vector.
#'
#' @name fun_stats
#' @docType methods
#' @export
#'
#' @param x A numeric vector.
#'
#' @return A list containing the minimum, maximum, mean, and median of the input vector.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' fun_stats(x)
#'
#'

setGeneric("fun_stats",
           def = function(x) {
             standardGeneric("fun_stats")
           })

setMethod("fun_stats",
          definition= function(x) {
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  mean <- mean(x, na.rm = TRUE)
  median<-median(x, na.rm = TRUE)
  summary <- list(min = min, max = max, mean = mean,median=median)
  return(summary)
})
