pacman::p_load(data.table,ggplot2,MASS,extRemes,
               copula,stargazer,
               lubridate,jtools,gamlss.dist,
               fitdistrplus,gamlss,purrr)
source("helperFuncs.R")
# Define your own data class
pBox<- setClass("pbox",slots = c(data = "data.table",copula="mvdc",fit=list()))
#new("pbox", data = SEAex, copula = copSEA)
# Define constructor function
setPBX <- function(data) {
  ############################################
  #+Questo dovrebbe sparare fuori una mega tabella
  #+con tutti i BIC/ AIC alla pycret
  ############################################
  if (!inherits(data, c("data.frame","data.table"))) {
    stop("Input must be a data frame or a data.table")
  }
  setDT(data)
  ### Fit distribution to extreme temp Malaysia,Thailand,Vietnam
  seaGev<-lapply(SEAex[,.(Malaysia,Thailand,Vietnam)],gevFuncs)
  names(seaGev)

  ## Fit distribution to avgRegion temp SEA
  fitAvg<-fitDist(SEAex$avgRegion,type="realline")
  fitAvg$fits
  model<-gamlssML(SEAex$avgRegion,family="SHASHo2")
  coefz<-data.table(model$mu,model$sigma,
                    model$nu,model$tau)
  names(coefz)<-model$parameters
  plot(density(rSHASHo2(length(SEAex$avgRegion),mu=coefz$mu,sigma= coefz$sigma,
                        nu=coefz$nu, tau=coefz$tau)))


  plot.ts(SEAex,plot.type = "single")
  cor(SEAex,method = "kendall")


  # Copula object
  u <- pobs(SEAex)
  #Fit copula
  thetaVal <- 2
  copula <- evCopula(family = 'gumbel',param=thetaVal,dim=ncol(SEAex))
  copula@parameters
  fit.ml <- fitCopula(copula, u, method="ml")
  #optimal val of Theta
  coef(fit.ml)
  summary(fit.ml)
  copula <- evCopula(family = 'gumbel',param=coef(fit.ml),dim=ncol(SEAex))

  #gf <- gofCopula(copula, x=as.matrix(SEAex))
  #gf
  allDistrs<-map_depth(seaGev,1,"Pars")
  allDistrs$avgSEA<-as.list(coefz)
  distList<-c(rep("evd",ncol(SEAex)-1),"SHASHo2")
  copSEA <- mvdc(copula, distList,
                 unname(allDistrs))
  print("pBox object genrated!")
  #pBox(data =data, copula=copSEA)
  # Create the object
  obj <- new("pbox", data =data, copula=copSEA)

  # Return the object
  #return(obj)
}

fun_stats <- function(x) {
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  mean <- mean(x, na.rm = TRUE)
  median<-median(x, na.rm = TRUE)
  summary <- list(min = min, max = max, mean = mean,median=median)
}


setMethod(f = "show",
          signature = "pbox",
          definition = function(object){
            cat("Probabilistic Box Object of class pbox\n")
            cat("\n||--General Overview--||")
            cat("\n----------------\n")
            cat("1)Data Structure\n")
            cat("Number of Rows: ", nrow(object@data), "\n")
            cat("Number of Columns: ", ncol(object@data), "\n")
            cat("\n")
            cat("1.1)Variable Statistics:\n")
            print(object@data[, rbindlist(lapply(.SD, fun_stats), idcol = "var")])
            cat("\n----------------\n")
            cat("2)Copula Summary:\n")
            print(object@copula@copula)
            cat("\n")
            cat("2.1)Copula margins:\n")
            print(object@copula@margins)
            cat("2.1)Kendall Kendall correlation:\n")
            print(cor(object@data,method = "kendall"))
            cat("\n-------------------------------\n")
            cat("You can now query the data as pbx['Var:Val',Var:Val',...]\n")
            cat("The data.table object can be directly access with pbx@data\n")
            cat("The copula object can be directly access with pbx@copula\n")
            cat("Please follow the instruction in xxx\n")

          })

q_parser<-function(query){
  # Define the regular expression pattern
  pattern <- "([a-zA-Z]+)(:)(\\d+)|(\\w+)([:])c\\(([^)]+)\\)"

  # Extract matches using the regular expression pattern
  matches <- as.data.table(stringr::str_match_all(query, pattern)[[1]])[, -1]
  colnames(matches) <- c("Varnames", "Colon1", "Value", "Operator", "Colon2", "Varnames2")
  matches$Value<-as.numeric(matches$Value)
  matches[,c('Colon1','Colon2'):=NULL]
  matches<-matches[, Filter(function(x) any(!is.na(x)), .SD)]

  return(matches)

}


statsCalc<- function(data, matches,varSet) {
  for (i in 1:nrow(matches)) {
    operator <- matches$Operator[i]
    varnames <- unlist(strsplit(matches$Varnames2[i], ","))

    if (operator == "mean") {
      result <- colMeans(data[, ..varnames])
    } else if (operator == "median") {
      result <- apply(data[,..varnames], 2, median)
    } else {
      stop("Unsupported operator. Only 'mean' and 'median' are supported.")
    }
    result<-as.data.table(result,keep.rownames ="Varnames")
    # Replace the matching values in the dataframe with the calculated result
    varSet[match(result$Varnames, varSet$Varnames),]<- result
  }

  return(varSet)
}

matchMaker<-function(varSet,matches,data){
  if ('Varnames' %in% names(matches)) {
    matchesVal<-na.omit(matches[,.(Varnames,Value)])
    varSet[match(matchesVal$Varnames, varSet$Varnames),]<-matchesVal
  }
  if('Varnames2' %in% names(matches)){
    matchesOp<-na.omit(matches[,.(Operator,Varnames2)])
    varSet<-statsCalc(data,matches=matchesOp,varSet)
  }
  return(varSet)
}

# Define method to subset data
# Defining the method for `[` for data.table objects

setMethod("[", signature = "pbox",
          definition = function(x="pbox",marginal="character",conditional="character", i="missing", j="missing", ..., lower.tail=TRUE,fixed=FALSE,drop="missing") {
          #definition = function(x,marginal,conditional, i, j, ..., lower.tail=TRUE,fixed=FALSE,drop) {
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

              varSet<-matchMaker(varSet,q_parser(marginal),x@data)
              probres<-pMvdc(varSet$Value, x@copula)
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

                varSet<-matchMaker(varSet,q_parser(z),x@data)
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
