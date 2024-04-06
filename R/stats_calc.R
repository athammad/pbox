##############################################################
#' Calculate basic statistics to be used to query the pbox object
#'
#' Internal function used to calculate mean and median as part of the query.
#'
#' @param data
#' @param matches
#' @param varSet
#'
#'
#'
#' @export
#' @import data.table
#' @import stringr str_match_all



stats_calc<- function(data, matches,varSet) {
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
}
