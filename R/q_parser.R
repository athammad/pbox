#' Parse Query
#'
#' Internal function used to parse a query to explore a pbox object.
#'
#' @name q_parser
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param query A string representing the query.
#'
#' @return A table with the parsed elements of the query.
#'
#' @examples
#' query <- "some_query_string"
#' q_parser(query)
#'
#' @importFrom data.table as.data.table
#' @importFrom stringr str_match_all


setGeneric("q_parser",
           def = function(query) {
             standardGeneric("q_parser")
           })

setMethod("q_parser",
          definition= function(query){
  # Define the regular expression pattern
  #pattern <- "([a-zA-Z]+)(:)(\\d+)|(\\w+)([:])c\\(([^)]+)\\)"
  pattern <- "([a-zA-Z]+)(:)(\\d+\\.?\\d*)|(\\w+)([:])c\\(([^)]+)\\)"
  # Extract matches using the regular expression pattern
  matches <- as.data.table(str_match_all(query, pattern)[[1]])[, -1]
  colnames(matches) <- c("Varnames", "Colon1", "Value", "Operator", "Colon2", "Varnames2")
  matches$Value<-as.numeric(matches$Value)
  matches[,c('Colon1','Colon2'):=NULL]
  matches<-matches[, Filter(function(x) any(!is.na(x)), .SD)]

  return(matches)

})
