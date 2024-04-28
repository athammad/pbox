##############################################################
#' Query the probabilistic space of a pbox object.
#'
#' This function queries the probabilistic space of a pbox object to calculate probabilities
#' associated with specific marginal or conditional distributions. It supports conditional
#' probability calculations and can optionally estimate confidence intervals through bootstrapping.
#'
#' @name qpbox
#' @export
#' @param x An object of class \code{pbox} from which to query the probabilistic space.
#' @param marginal A character string specifying the marginal and joint distribution of the variable.
#'        It must specify the variable and the value in the format 'Var:Val'.
#' @param conditional A character string specifying the marginal and conditional distribution of the variable.
#'        It must specify the variable and the value in the format 'Var:Val'.
#' @param lower.tail Logical; if TRUE (default), probabilities are calculated for the area to the right of the specified value.
#' @param fixed Logical; if TRUE, calculates conditional probabilities with conditions treated as fixed.
#' @param CI Logical; if TRUE, calculates bootstrap confidence intervals.
#' @param iter Integer; the number of replications for the confidence interval calculation. Default is 1000.
#' @return Estimated probabilities as a numeric value or a named vector including confidence intervals if requested.
#' @examples
#' \dontrun{
#'   data("SEAex")
#'   pbx <- set_pbox(SEAex)
#'   # Get marginal distribution
#'   qpbox(pbx, marginal="Malaysia:33")
#'   # Get conditional distribution with fixed conditions
#'   qpbox(pbx, marginal="Malaysia:33 & Vietnam:31", conditional="avgRegion:26", fixed=TRUE)
#' }
#' @importFrom copula pMvdc cCopula
#' @importFrom data.table setDT
#' @importFrom stats setNames

setGeneric("qpbox",
           def = function(x,marginal="character",conditional="character", lower.tail=TRUE,fixed=FALSE,CI=FALSE,iter=1000) {
             standardGeneric("qpbox")
           })


#' @rdname qpbox
#' @description
#' This method processes the \code{pbox} object to compute probabilities based on the specified marginal
#' and conditional parameters. It handles both simple probability calculations and complex queries involving
#' joint and conditional distributions, with an option for bootstrap confidence interval estimation.

setMethod("qpbox", signature = "pbox",
          definition = function(x,marginal="character",conditional="character", lower.tail=TRUE,fixed=FALSE,CI=FALSE,iter=1000) {
            #definition = function(x,marginal,conditional, i, j, ..., lower.tail=TRUE,fixed=FALSE,drop) {

            if (!inherits(x, c("pbox"))) {
              stop("Input must be a pbox object!")
            }

            Varnames<-names(x@data)
            Value<-rep(Inf,ncol(x@data))
            varSet<-cbind.data.frame(Varnames,Value)
            # Perform subsetting and other operations here
            # Use `marginal`, `conditional`, and other arguments as needed

            # Example: Subsetting rows and columns
            if (missing(conditional)) {
              # If only `marginal` is provided, subset rows
              marginal<- gsub("[[:blank:]]", "",marginal)
              if (!grepl(":", marginal)) {
                stop("Please specify the variable and the value in the following format 'Var:Val'")
              }
              if (!is.character(marginal)) {
                stop("Expecting a string to query the pbox!")
              }

              if(CI){
                varSet<-match_maker(varSet,q_parser(marginal),x@data)
                res<-pMvdc(c(varSet$Value), x@copula)
                probres<-probCI(replicate(iter, perProb(x,varSet$Value)))
                probres<-c(res,probres)
                if(lower.tail==FALSE){probres<-1-probres}
                probres<-setNames(probres, c("P", "2.5%", "97.5%"))
                probres
              }else{
              #browser()
              varSet<-match_maker(varSet,q_parser(marginal),x@data)
              probres<-pMvdc(c(varSet$Value), x@copula)
              if(lower.tail==FALSE){probres<-1-probres}
              probres <- setNames(probres, "P")
              probres
              }
            } else {
              # If both `marginal` and `conditional` are provided, subset rows and select columns
              cond<-lapply(list(marginal,conditional),function(z){
                z<-gsub("[[:blank:]]", "",z)
                if (!grepl(":", z)) {
                  stop("Please specify the variable and the value in the following format 'Var:Val'")
                }
                if (!is.character(z)) {
                  stop("Expecting a string to query the pbox!")
                }

                if(CI){
                  varSet<-match_maker(varSet,q_parser(z),x@data)
                  res<-pMvdc(c(varSet$Value), x@copula)
                  probres<-probCI(replicate(iter, perProb(x,varSet$Value)))
                  probres<-c(res,probres)
                  probres<-setNames(probres, c("P", "2.5%", "97.5%"))
                  probres
                }else{
                  varSet<-match_maker(varSet,q_parser(z),x@data)
                  #query copula
                  p<-pMvdc(varSet$Value,x@copula)
                  p <- setNames(p, "P")
                  p
                }
              })
              if (fixed) {
                condFix <- cCopula(
                  cbind(cond[[1]], cond[[2]]),
                  indices = 2,
                  copula = x@copula@copula)
                if(lower.tail==FALSE){condFix<-1-condFix}
                condFix<-as.vector(condFix)
                if (length(condFix) == 1) {
                  condFix <- setNames(condFix, "P")
                } else {
                  condFix <- setNames(condFix, c("P", "2.5%", "97.5%"))
                }
                condFix
              }
              else{
                probrez<-cond[[1]]/cond[[2]]
                if(lower.tail==FALSE){probrez<-1-probrez}
                if (length(probrez) == 1) {
                  probrez <- setNames(probrez, "P")
                } else {
                  probrez <- setNames(probrez, c("P", "2.5%", "97.5%"))
                }
                probrez
              }}

          })


