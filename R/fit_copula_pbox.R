##############################################################
#' Copula Fit
#'
#' Method to automatically find the best Copula given a data.frame. Wrapper around the function \code{fitCopula}.
#'
#' @param data A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).

#' @param copula_families List of copula types  and their corresponding families Currently \bold{pbox} supports only Archimedean, Elliptical and Extreme-Value copula.
#' Currently supported families are "clayton", "frank", "amh", "gumbel", and "joe" for Archimedean Copula.
#' "galambos", "gumbel" and "huslerReiss" for Extreme-Value copula.
#' "normal" and "t" for Elliptical copula.
#'
#' @return a data.table with the correspondent AIC and the parameter estimates of the evaluated copulas and families.
#'
#'
#'
#' @export
#' @examples
#' SEAex<-fread("./data/SEAex.csv")
#' # Define the copula families and their corresponding parameters
#'  copula_families <- list(
#'  archmCopula = c("clayton", "frank", "gumbel", "joe"),# "amh",
#'  evCopula = c("galambos", "gumbel", "huslerReiss" ),#"tawn" #"tev"
#'  ellipCopula = c("normal")# "t"
#')
#' distFits<- fit_copula_pbox(data=SEAex,copula_families)
#' distFits
#'
#' @import data.table
#' @importFrom copula pobs fitCopula archmCopula evCopula ellipCopula coef
#' @import gamlss
#' @importFrom purrr map_depth



fit_copula_pbox<-function(data,copula_families){

  u <- copula::pobs(data)
  dfCopula <- stats::setNames(utils::stack(copula_families), c('family','copula'))

  # Perform grid search
  results <- apply(dfCopula, 1, function(row) {
    fit_copula(copula=row["copula"], family=row["family"], dim = ncol(data), u)

  })
  # Convert results to data frame
  results_df <- do.call(rbind, results)
  return(results_df)
}

# Define the copula families and their corresponding parameters
copula_families <- list(
  archmCopula = c("clayton", "frank", "gumbel", "joe"),# "amh",
  evCopula = c("galambos", "gumbel", "huslerReiss" ),#"tawn" #"tev"
  ellipCopula = c("normal")# "t"
)

fit_copula <- function(copula, family, dim, u) {
  if (dim > 2 && family %in% c("amh", "galambos", "huslerReiss", "tawn", "tev")) {
    return(NULL)  # Skip if the family is not available for dim > 2
  }
  copFun <-getFromNamespace(copula,ns = "copula")
  cop <- copFun(family = family, param = NA_real_, dim = dim)


  fit <- copula::fitCopula(cop, u, method = "ml")
  aicVal <- stats::AIC(fit)
  coefVal <- copula::coef(fit)

  return(data.table(copula=copula,family = family, AIC = aicVal, coef = coefVal))
}
