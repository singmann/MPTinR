\name{parameters-methods}
\docType{methods}
\alias{parameters-methods}
\alias{parameters}
\alias{parameters,bmpt-method}
\alias{parameters,mpt-method}
\title{Parameter values for MPT models}
\description{
Generic function to display the parameter values and confidence intervals for MPT models.
}
\usage{
\S4method{parameters}{mpt}(object, ci = NULL, show.equality = FALSE, show.fixed = TRUE, sort.alphabetical = FALSE, ...)
\S4method{parameters}{bmpt}(object, type = c("nonparametric", "parametric", "fisher"), ci = NULL, show.equality = FALSE, show.fixed = TRUE, sort.alphabetical = FALSE, ...)
}
\arguments{
  \item{object}{object of class \code{mpt} or \code{bmpt}}
  \item{type}{type of confidence interval to be displayed. Other types than fisher are only possible for objects of class \code{bmpt} and when the relevant calculations have already been done (which are currently not implemented).}
  \item{ci}{numeric or \code{NULL}. Size of confidence interval. If \code{NULL} (the default) the value is taken from objects's slot \code{default.ci} (default = 95).}
  \item{show.equality}{Logical. Should all parameters that are euqal to other be displayed? If \code{FALSE} (the default) only the parameter that is estimated is displayed and all that are equal to this one ommited.}
  \item{show.fixed}{Logical. Should parameters that are restricted to a value be displayed?}
  \item{sort.alphabetical}{Logical. Should parameters be orderd alphabetical? If \code{FALSE} (the default), the order of parameters is: 1. free parameters, 2. inequality restricted parameters, 3. equality restricted parameters, 4. fixed.parameters.}
  \item{...}{ignored.}
}
\details{
Convenience function that displayes parameter values plus confidence intervals. Parameters that are restricted are marked in the output (1 = fixed parameter, 2 = equality restricted parameter, 3 = inequality restricted parameter).
}
\value{
For a signle fit a data.frame with four columns: estimated parameter values, lower bound of the confidence interval, upper bound of the confidence interval, type of parameter.

For mutliple fits a list with three items. The columns are the same as for single fits.
\item{individual}{A 3-dimensional (named) array with the value for each individual, with dimensions: \code{(parameter, column, individual)}}
\item{mean}{mean parameter values}
\item{aggregated}{values for the aggregated data}
}
\author{
Henrik Singmann
}
\seealso{\code{\link{fit.mpt}} fir the function that returns the necessary \code{object}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
