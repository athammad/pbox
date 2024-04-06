##############################################################
#' Generate the query vector to explore the probabilistc space.
#'
#' Internal function used to Parse query to explore a pbox object.
#'
#' @param varSet string
#' @param matches description
#' @param data description
#' @return A table with the parsed elements of the query.
#'
#'
#'
#' @export
#' @import data.table
#' @import stringr str_match_all
#'


match_maker<-function(varSet,matches,data){
  if ('Varnames' %in% names(matches)) {
    matchesVal<-na.omit(matches[,.(Varnames,Value)])
    varSet[match(matchesVal$Varnames, varSet$Varnames),]<-matchesVal
  }
  if('Varnames2' %in% names(matches)){
    matchesOp<-na.omit(matches[,.(Operator,Varnames2)])
    varSet<-stats_calc(data,matches=matchesOp,varSet)
  }
  return(varSet)
}
