##############################################################
#' Query the probabilistic space of a pbox object.
#'
#' Combine all the results from \code{fit_copula_pbox} and \code{fit_dist_pbox} to build a Multivariate Distributions from Copula.
#'
#' @name qpbox
#' @docType methods
#' @export
#' @include pbox.R
#' @aliases qpbox
#'
#' @param x object of class \bold{pbox} from which to query the probabilistic space.
#' @param marginal character; string used to query the marginal and joint distribution of the variable. Must specify the variable and the value in the following format 'Var:Val'
#' @param conditional character; string used to query the marginal and conditional distribution of the variable. Must specify the variable and the value in the following format 'Var:Val'
#' @param lower.tail logical; if TRUE (default), probabilities are calculated for the area to the right of the specified value.
#' @param fixed logical; if TRUE (default), probabilities are \eqn{P(X \leq x | Y = y)}
#' @param CI logical; if TRUE, calculate bootstrap confidence intervals. See notes for details.
#' @param iter number of replications for the confidence interval calculation. 1000 by default.
#'
#' @return Estimated probability.
#'
#' @examples
#' data("SEAex")
#' pbx<-set_pbox(SEAex)
#' #Get marginal distribution
#' qpbox(pbx,marginal="Malaysia:33",)
#' #Get Joint distribution
#' qpbox(pbx,marginal="Malaysia:33 & Vietnam:34",)
#' #Get Joint distribution
#' qpbox(pbx,marginal="Vietnam:31", conditonal="avgRegion:26")
#' #Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y <= y)
#' qpbox(pbx,marginal=Malaysia:33 & Vietnam:31", conditonal="avgRegion:26")
#' #Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y = y)
#' qpbox(pbx,"Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE)
#' # Joint distribution with values set on their respective mean value
#' qpbox(pbx,"mean:c(Vietnam,Thailand)",lower.tail=TRUE)
#' # Joint distribution with values set on their respective median value
#' qpbox(pbx,"median:c(Vietnam, Thailand)",lower.tail=TRUE)
#' # Joint distribution with xxxx
#' qpbox(pbx,"Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=TRUE)
#' # Condtional distribtuion distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
#' qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)")
#' # Condtional distribtuion distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
#' qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)",CI=TRUE,iter=100)
#' @importFrom copula pMvdc cCopula
#' @importFrom stats setNames
#'
#'



setGeneric("qpbox",
           def = function(x,marginal="character",conditional="character", lower.tail=TRUE,fixed=FALSE,CI=FALSE,iter=1000) {
             standardGeneric("qpbox")
           })

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


