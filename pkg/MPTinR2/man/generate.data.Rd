\name{generate.data}
\docType{methods}
\alias{generate.data}
\alias{generate.data-methods}
\alias{generate.data,mpt-method}
\alias{generate.data,mpt.model-method}
\title{Generate data from MPT models.}
\description{
\code{generate.data} in \pkg{MPTinR} is a generic function that generates random data or predicted data for MPT models from given parameter values.
}
\usage{
\S4method{generate.data}{mpt.model}(model, parameter.values, n.per.tree, n = 1, random = TRUE, ...)
\S4method{generate.data}{mpt}(model, n = 1, random = TRUE, ...)
}

\arguments{
  \item{model}{
Either a MPT model (an object of class \code{bmpt.model} or \code{mpt.model}, returned from \code{\link{make.mpt}}) or a fitted MPT model (object of class \code{bmpt} or \code{mpt}, returned from \code{\link{fit.mpt}})
}
  \item{parameter.values}{
\emph{named} vector or matrix with the parameter estiamtes to be used for data generation. If a vector, \code{names(parameter.values)} must correspond to the parameters in the model. If a matrix, each row represents one set of parameter estimates and the \code{colnames(parameter.values)} must correspond to the parameters in the model.
}
\item{n.per.tree}{
vector or matrix with length/ number of columns corresponding to the number of categories in the model. Each value must correspond to the n per tree for that category. If a matrix, each row represents one set of ns.
}
  \item{n}{
numeric. Number of random samples to be drawn per set of parameter values (ignored if \code{random = FALSE}).
}
  \item{random}{
logical. If \code{TRUE} (the default) random samples from a multinomial distribution are drawn. If \code{FALSE} the values predicted from the given parameter estimates are returned (and argument \code{n} is ignored).
}
  \item{...}{currently ignored.}
}
\details{
This function generates data from either a mutlinomial processing tree (MPT) model object as produced by \code{\link{make.mpt}} (which is either of class \code{bmpt.model} or \code{mpt.model}) or a fitted MPT model object as produced by \code{\link{fit.mpt}} (which is either of class \code{ bmpt} or \code{mpt}).

To generate data from a MPT models you also need to specify parameter values and the desired n. Both can be vectors to generate data from a single individual or a matrix to generate data from multiple individuals with each row corresponding to a single individual. The parameter values need to ba a named object, either a vector or a matrix. The \code{names} for a vector or the \code{colnames} of a matrix need to correspond to the free parameters in the model. Additional parameters or named objects not part of the model should be ignored.
}
\value{
If \code{random} is TRUE, a 3-dimensional array with \code{dim(n, n.categories, n.parameter.sets)}:
\item{first dimension}{the number of random draws for each set of parameters (i.e., = \code{n}).}
\item{second dimension}{the response categories in the model}
\item{third dimension}{the parameter set (i.e., the number of the row in the \code{parameter.values} object or the dataset)}

If \code{random} is FALSE, a vector or matrix containing the (non-rounded) expected values.
}
\author{
Henrik Singmann
}
\seealso{\code{\link{fit.mpt}}, \code{\link{make.mpt}}}
\keyword{methods}

