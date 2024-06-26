% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probCI.R
\docType{methods}
\name{probCI}
\alias{probCI}
\alias{probCI-method}
\title{Probability Confidence Interval}
\usage{
probCI(probabilities, alpha=0.05)
}
\arguments{
\item{probabilities}{A numeric vector of probabilities for which the confidence interval is desired.}

\item{alpha}{The significance level used for constructing the confidence interval; default is 0.05.}
}
\value{
A list containing the lower and upper bounds of the confidence intervals for each probability.
}
\description{
Calculates the confidence interval around a vector of probabilities
using the quantiles based on the specified significance level.
}
\examples{
probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5)
probCI(probabilities)
probCI(probabilities, alpha = 0.1)
}
