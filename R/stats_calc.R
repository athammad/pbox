##############################################################
#' Calculate basic statistics to be used to query the pbox object
#'
#' Internal function used to calculate mean and median as part of the query.
#'
#' @name stats_calc
#' @docType methods
#' @export
#' @include pbox.R
#'
#'
#' @param data data A \code{data.frame} or \code{data.table}
#' @param matches \code{data.frame} with user query
#' @param varSet \code{data.frame} with variable names

setGeneric("stats_calc",
           def = function(data, matches,varSet) {
             standardGeneric("stats_calc")
           })

setMethod("stats_calc",
          definition=function(data, matches,varSet) {
  for (i in 1:nrow(matches)) {
    operator <- matches$Operator[i]
    varnames <- unlist(strsplit(matches$Varnames2[i], ","))

    if (operator == "mean") {
      result <- colMeans(data[, ..varnames])
    } else if (operator == "median") {
      result <- apply(data[,..varnames], 2, median)
    } else {
      stop("Unsupported operator. Only 'mean' and 'median' are supported.")
    }
    result<-as.data.table(result,keep.rownames ="Varnames")
    # Replace the matching values in the dataframe with the calculated result
    varSet[match(result$Varnames, varSet$Varnames),]<- result
  }

  return(varSet)
})
