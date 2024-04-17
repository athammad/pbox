##############################################################
#' Methods for 'show()' in Package 'pbox'
#'
#' Methods for function show in package \blod{pbox}.
#'
#' @export
#' @name show
#' @docType method
#' @include pbox.R

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
            cat("2.1)Kendall correlation:\n")
            print(cor(object@data,method = "kendall"))
            cat("\n-------------------------------\n")

          })


