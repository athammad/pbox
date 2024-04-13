##############################################################
#' Query the probabilistic space of a pbox object.
#'
#' Combine all the results from \code{fit_copula_pbox} and \code{fit_dist_pbox} to build a Multivariate Distributions from Copula.
#'
#' @name qpbox
#' @docType methods
#' @export
#
#' @import data.table
#' @import copula
#' @include pbox.R
#'
#' @param x object of class \bold{pbox} from which to query the probabilistic space.
#' @param marginal character; string used to query the marginal and joint distribution of the variable. Must specify the variable and the value in the following format 'Var:Val'
#' @param conditional character; string used to query the marginal and conditional distribution of the variable. Must specify the variable and the value in the following format 'Var:Val'
#' @param lower.tail logical; if TRUE (default), probabilities are $$
#' @param fixed logical; if TRUE (default), probabilities are $$
#' @return An object of class \code{mvdc}.
#'
#' @examples
#' data("SEAex")
#' pbx<-set_pbox(SEAex)
#' #Get marginal distribution
#' pbx["Malaysia:33",]
#' #Get Joint distribution
#' pbx["Malaysia:33 & Vietnam:34",]
#' #Get Joint distribution
#' pbx["Vietnam:31", "avgRegion:26"]
#' #Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y <= y)
#' pbx["Malaysia:33 & Vietnam:31", "avgRegion:26"]
#' #Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y = y)
#' pbx["Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE]
#' # Joint distribution with values set on their respective mean value
#' pbx["mean:c(Vietnam,Thailand)",lower.tail=T]
#' # Joint distribution with values set on their respective median value
#' pbx["median:c(Vietnam, Thailand)",lower.tail=T]
#' # Joint distribution with xxxx
#' pbx["Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=T]
#' # Condtional distribtuion distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
#' pbx["Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)"]
#'

setGeneric("qpbox",
           def = function(x,marginal="character",conditional="character", lower.tail=TRUE,fixed=FALSE) {
             standardGeneric("qpbox")
           })

setMethod("qpbox", signature = "pbox",
          definition = function(x,marginal="character",conditional="character", lower.tail=TRUE,fixed=FALSE) {
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

              varSet<-match_maker(varSet,q_parser(marginal),x@data)
              probres<-pMvdc(c(varSet$Value), x@copula)
              if(lower.tail==FALSE){probres<-1-probres}
              as.vector(probres)

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

                varSet<-match_maker(varSet,q_parser(z),x@data)
                #query copula
                p=pMvdc(varSet$Value, x@copula)
                as.vector(p)
              })
              if (fixed) {
                condFix <- cCopula(
                  cbind(cond[[1]], cond[[2]]),
                  indices = 2,
                  copula = x@copula@copula)
                if(lower.tail==FALSE){condFix<-1-condFix}
                as.vector(condFix)

              }
              else{
                probrez<-cond[[1]]/cond[[2]]
                if(lower.tail==FALSE){probrez<-1-probrez}
                as.vector(probrez)
              }}

          })
