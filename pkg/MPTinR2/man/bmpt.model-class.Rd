\name{bmpt.model-class}
\Rdversion{1.1}
\docType{class}
\alias{bmpt.model-class}
\alias{mpt.model-class}
\alias{check}
\alias{check,mpt.model-method}
\alias{show,mpt.model-method}
\alias{initial.model.data.frame}
\alias{initial.model.data.frame,mpt.model-method}
\alias{model.data.frame}
\alias{model.data.frame,mpt.model-method}
\alias{lbmpt,bmpt.model-method}


\title{Class \code{"bmpt.model"}}
\description{
\code{bmpt.model} and \code{mpt.model} are two classes representing a multinomial processing tree (MPT) model. \code{mpt.model} is the superclass and \code{bmpt.model} a subclass for all those MPT models that are members of L-BMPT, the context-free language for MPT models (Purdy & Batchelder, 2009). 
}
\section{Objects from the Class}{
Objects can be created by calling \code{\link{make.mpt}}.

For a MPT to be a BMPT (or a member of L-BMPT), it is \emph{absolutely necessary} that the equations representing the model perfectly map the tree structure of the (binary) MPT. That is, the model file is only allowed to contain parameters, their negations (e.g., \code{Dn} and \code{(1 - Dn)}) and the operators + and *, but nothing else. Simplifications of the equations or a change in the order of the parameters will lead to an object of class \code{mpt.model}. Models represented as \code{mpt.model}s have numerous disadvantages: Fitting is a lot slower, you cannot compute the FIA, ...

To take full advantage of \pkg{MPTinR} specify a \code{bmpt.model}.
}
\section{Slots}{
  \describe{
    \item{\code{check}:}{List containing various information about the model (e.g., parameters, degrees of freedom,...). }
    \item{\code{initial.model.data.frame}:}{data.frame representation of the initial model (i.e., no restrictions applied). Basically a representation in EQN syntax plus a column indicating the branch number.}
    \item{\code{model.data.frame}:}{data.frame representation of the final model (i.e., restrictions applied). Basically a representation in EQN syntax plus a column indicating the branch number.}
    \item{\code{initial.model}:}{List representation of the inital model's equations (i.e., no restrictions applied).}
    \item{\code{model.list}:}{List representation of the final model's equations (i.e., restrictions applied).}
    \item{\code{restrictions}:}{List containing the restrictions (uses non-visible classes for representation of restrictions).}
    \item{\code{A}:}{So called \code{A} matrix. Three-dimensional array representing the position of the non-inverted parameters (e.g., \code{Dn}) in the model. First dimension = response category, second dimension = branch for that response category, third dimension = parameter (in alphabetical order). Only in \code{bmpt.model}s.}
    \item{\code{B}:}{So called \code{B} matrix. Three-dimensional array representing the position of the inverted parameters (e.g., \code{(1 - Dn)}) in the model. First dimension = response category, second dimension = branch for that response category, third dimension = parameter (in alphabetical order). Only in \code{bmpt.model}s.}
    \item{\code{lbmpt}:}{L-BMPT representation of the model.  Only in \code{bmpt.model}s.}
  }
}
\section{Extends}{
\code{bmpt.model} extends \code{mpt.model}.
}
\section{Methods}{
  \describe{
    \item{check}{\code{signature(object = "mpt.model")}: Very informative function that returns a list with useful information for a model: \cr
Do probabilities sum to 1?; number of trees; number of categories; number of free parameters in the final model; names of free parameters in the final model; number of fixed parameters in the final model; names of fixed parameters in the final model; names of parameters in the initial model (i.e., no restrictions applied); maximum number of branches per category; vector with number of branches per category; degrees of freedom; is model a member of L-BMPT?\cr
Is part of the \code{show} method for model objects.}
    \item{initial.model.data.frame}{\code{signature(object = "mpt.model")}: Returns a data.frame representing the initial model (i.e., no restrictions applied).}
    \item{model.data.frame}{\code{signature(object = "mpt.model")}: Returns a data.frame representing the final model (i.e., restrictions applied).}
    \item{lbmpt}{\code{signature(object = "bmpt.model")}: returns a character vector with the L-BMPT representation.}
	 }
}
\references{
Purdy, B. P., & Batchelder, W. H. (2009). A context-free language for binary multinomial processing tree models. \emph{Journal of Mathematical Psychology}, 53, 547-561.

See the following reference for more on \code{A} and \code{B} matrices: \cr
Hu, X., & Batchelder, W. H. (1994). The statistical analysis of general processing tree models with the EM algorithm. \emph{Psychometrika}, 59(1), 21-47.

}
\author{
Henrik Singmann
}

\seealso{
\code{\link{make.mpt}}, \code{\link{fit.mpt}}.
}
\examples{
showClass("bmpt.model")
}
\keyword{classes}
