##############################################################
#' Create a probability box from data.
#'
#' Method to automatically find the best marginal distribution and copula for a give dataset.
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param ... Other arguments to be passed to \code{fitDist}.
#' @return an object of class \code{pbox} with 3 slots:
#'
#' \code{data}: The original data coerced to a data.table.
#'
#' \code{copula}: The copula object of class \code{mvdc}.
#'
#' \code{fit}: The results of the automated selection for both the marginal distribution and the copula.
#'
#'
#' @export
#' @examples
#' SEAex<-fread("./data/SEAex.csv")
#' pbx<- set_pbox(data=SEAex)
#' pbx
#' class(pbx)
#'
#' @import data.table
#' @import copula
#' @import gamlss.dist
#' @import gamlss
#' @import purrr


set_pbox <- function(data,...) {

  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }
  setDT(data)
  distSearch<-fit_dist_pbox(data)
  CopulaSearch<-fit_copula_pbox(data,copula_families)
  ##
  ##
  ##
  ##

  finalCopula<-final_pbox(CopulaSearch,distSearch$allDitrs,data)
  print("pbox object genrated!")

  obj <- new("pbox", data =data, copula=finalCopula,fit=list(distSearch,CopulaSearch))

}

