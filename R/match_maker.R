#' Generate Query Vector
#'
#' Internal function used to generate the query vector to explore the probabilistic space.
#'
#' @name match_maker
#' @docType methods
#' @export
#' @include pbox.R
#'
#' @param varSet A string describing the variable set.
#' @param matches A description of matches.
#' @param data A description of the data.
#'
#' @return A table with the value to be queried of each column/marginal distribution of the data.
#'
#' @importFrom stats na.omit

setGeneric("match_maker",
           def = function(varSet,matches,data) {
             standardGeneric("match_maker")
           })

setMethod("match_maker",
          definition= function(varSet,matches,data){
  if ('Varnames' %in% names(matches)) {
    matchesVal<-na.omit(matches[,.(Varnames,Value)])
    varSet[match(matchesVal$Varnames, varSet$Varnames),]<-matchesVal
  }
  if('Varnames2' %in% names(matches)){
    matchesOp<-na.omit(matches[,.(Operator,Varnames2)])
    varSet<-stats_calc(data,matches=matchesOp,varSet)
  }
  return(varSet)
})
