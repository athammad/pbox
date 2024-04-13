##############################################################
#' Class "pbox": Main S4 class of the library \bold{pbox}.
#'
#' "pbox" is a class representing the probabilistc space which combines data, copula and margins.
#'
#' @export
#' @name pbox-class
#' @docType class
#' @slot data The original data coerced to a \code{data.table}.
#' @slot copula The copula object of class \code{mvdc}.
#' @slot fit The results of the automated selection for both the marginal distribution and the copula.
#' @import data.table
#' @import copula
#'
setClass("pbox",
         slots = c(data = "data.table",
                   copula="mvdc",
                   fit="list"))