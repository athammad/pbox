##############################################################
#' Parse query to explore a pbox object
#'
#' Internal function used to Parse query to explore a pbox object.
#'
#' @param query string
#' @return A table with the parsed elements of the query.
#'
#'
#'
#' @export
#' @import data.table
#' @import stringr str_match_all

q_parser<-function(query){
  # Define the regular expression pattern
  #pattern <- "([a-zA-Z]+)(:)(\\d+)|(\\w+)([:])c\\(([^)]+)\\)"
  pattern <- "([a-zA-Z]+)(:)(\\d+\\.?\\d*)|(\\w+)([:])c\\(([^)]+)\\)"
  # Extract matches using the regular expression pattern
  matches <- as.data.table(stringr::str_match_all(query, pattern)[[1]])[, -1]
  colnames(matches) <- c("Varnames", "Colon1", "Value", "Operator", "Colon2", "Varnames2")
  matches$Value<-as.numeric(matches$Value)
  matches[,c('Colon1','Colon2'):=NULL]
  matches<-matches[, Filter(function(x) any(!is.na(x)), .SD)]

  return(matches)

}
