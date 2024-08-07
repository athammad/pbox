% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_pbox.R
\docType{methods}
\name{set_pbox}
\alias{set_pbox}
\alias{set_pbox,ANY-method}
\title{Create a Probability Box from Data}
\usage{
set_pbox(data, verbose = TRUE, ctype = "all", cfamily = "all", ...)

\S4method{set_pbox}{ANY}(data, verbose = TRUE, ctype = "all", cfamily = "all", ...)
}
\arguments{
\item{data}{A data frame or data table. The data will be coerced to a `data.table` internally.}

\item{verbose}{control verbosity of the output. Default to TRUE.}

\item{ctype}{Charter indicating the type of copula among archmCopula,evCopula,ellipCopula.}

\item{...}{Other arguments to be passed to the `fitDist` function.}

\item{cfamilly}{Charter indicating the family of copula among clayton,frank,gumbel,joe,galambos, huslerReiss, normal.}
}
\value{
An object of class `pbox` with the following slots:
        - `@data`: The original data coerced into a `data.table`.
        - `@copula`: The selected copula object, typically of class `mvdc`.
        - `@fit`: A list containing results from the automated selection processes for
          both the marginal distributions and the copula.
}
\description{
Constructs a probability box (pbox) by automatically selecting the best marginal distribution
and copula for a given dataset. This function facilitates the creation of a pbox object,
which encapsulates the uncertainty and dependencies of the input data.

`set_pbox` method that utilizes data frames or data tables to configure a comprehensive
pbox structure. The method involves stages of distribution fitting and copula selection,
executed through external functions presumed to be available in the working environment
or described in the package.
}
\examples{
  data("SEAex")
  pbx <- set_pbox(data = SEAex)
  print(pbx)
  print(class(pbx))
}
