##############################################################
#' Make a pbox from a data.frame and a copula object.
#'
#' Method to allow users to query a copula object created by the,
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param copula
#' @return an object of class \code{pbox} with 2 slots:
#'
#' \code{data}: The original data coerced to a data.table.
#'
#' \code{copula}: The copula object of class \code{mvdc}.
#'
#'
#' @export
#' @examples
#' SEAex<-fread("./data/SEAex.csv")
#'
#' pbx<- set_pbox(data=SEAex)
#' pbx
#' class(pbx)
#'
#' @import data.table
#' @import copula
#' @import gamlss.dist
#' @import gamlss
#' @import purrr
#'
#'

make_pbox<-function(data,copula){


  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }
  if (!inherits(copula, c("mvdc"))) {
    stop("Input must be an object of class mvdc genrated using the copula package")
  }

  # ADD checks for colnames, number of marginal distributions
  setDT(data)

  obj <- new("pbox", data =data, copula=copula,fit=list())

}

