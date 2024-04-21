#' Define Copula Families and Parameters
#'
#' Internal list of defined copula families and their corresponding parameters.
#'
#' @name define_copula_families
#' @export
#'
#'
.copula_families <- list(
  archmCopula = c("clayton", "frank", "gumbel", "joe"), # Archimedean copula families # "amh",
  evCopula = c("galambos", "gumbel", "huslerReiss"),     # Extreme-Value copula families #"tawn" #"tev"
  ellipCopula = c("normal")                              # Elliptical copula families # "t"
)

