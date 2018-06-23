\name{fit.mpt}
\docType{methods}
\alias{fit.mpt}
\alias{fit.mpt-methods}
\alias{fit.mpt,bmpt.model-method}
\alias{fit.mpt,character-method}
\alias{fit.mpt,mpt.model-method}
\title{
Function to fit MPT models
}
\description{
This generic function fits \emph{binary} multinomial processing tree models (MPT models; e.g., Riefer & Batchelder, 1988) from an external model file and (optional) external restrictions. Additionally, measures for model selection (AIC, BIC, FIA) can be computed.
}
\usage{
\S4method{fit.mpt}{character}(model, data, restrictions.filename = NULL, model.type = c("easy", "eqn", "eqn2"), start.parameters = NULL, ...)
\S4method{fit.mpt}{bmpt.model}(model, data, ci = 95, n.optim = list("auto", 5), start.parameters = NULL, ...)
\S4method{fit.mpt}{mpt.model}(model, data, n.optim = 5, ci = 95, start.parameters = NULL, method = c("L-BFGS-B", "nlminb"), multicore = c("none", "individual", "n.optim"), sfInit = FALSE, nCPU = 2, ...)
}

\arguments{
  \item{model}{
Either a \code{character} string specifying the position of the model filename or a model object of class \code{bmpt.model} or \code{mpt.model} as produced by \code{\link{make.mpt}}.
}
  \item{data}{
Either a \emph{numeric} \code{vector} for individual fit or a \emph{numeric} \code{matrix} or \code{data.frame} for multi-individual fit. The data on each position (column for multi-individual fit) must correspond to the respective response category (e.g., line in model file).
}
  \item{restrictions.filename}{
\code{NULL} or a character \code{vector} specifying the location and name of the restrictions file. Default is \code{NULL} which corresponds to no restrictions.
}
  \item{model.type}{
Character vector specifying whether the model file is formatted in the easy way (\code{"easy"}; i.e., each line represents all branches corresponding to a response category) or the traditional EQN syntax (\code{"eqn"} or \code{"eqn2"}; see \code{\link{make.mpt}}). If \code{model.filename} ends with .eqn or .EQN, \code{model.type} is automatically set to \code{"eqn"}. Default is \code{"easy"}.
}
  \item{start.parameters}{
A \code{numeric vector} or \code{NULL}. See Details.
}
  \item{ci}{
A scalar corresponding to the size of the default confidence intervals for the parameter estimates. Default is 95 which corresponds to 95\% confidence intervals. See also \code{\link{parameters-methods}}.
}
  \item{n.optim}{
List or numeric. Number of optimization runs. See Details
}
  \item{method}{
character. Only relevant for models not members of L-BMPT (see \code{\link{make.mpt}}), when fitting objects of class \code{mpt.model} (see also \code{\link{bmpt.model-class}}). \code{"L-BFGS-B"} is a quasi-Newton gradient-based general purpose optimization routine (see \code{\link{optim}}. \code{"nlminb"} is a gradient-based optimization routine that uses the PORT routines (see \code{\link{nlminb}}).
}

  \item{multicore}{
Character vector. If not \code{"none"}, uses \code{snowfall} for parallelization (which needs to be installed separately via \code{install.packages(snowfall)}). If \code{"individual"}, parallelizes the optimization for each individual (i.e., data needs to be a \code{matrix} or \code{data.frame}). If \code{"n.optim"}, parallelizes the \code{n.optim} optimization runs. Default is \code{"none"} which corresponds to no parallelization. Note that you need to initialize \code{snowfall} in default settings. See \code{sfInit} and Details.
}
  \item{sfInit}{
Logical. Relevant if \code{multicore} is not \code{"none"}. If \code{TRUE}, \code{fit.mpt} will initialize and close the multicore support. If \code{FALSE}, (the default) assumes that \code{sfInit()} was initialized before. See Details.
}
  \item{nCPU}{
Scalar. Only relevant if \code{multicore} is not \code{"none"} and \code{sfInit} is TRUE. Number of CPUs used by \code{snowfall}. Default is 2.
}
  \item{...}{
Used to pass arguments from the method for \code{character} to the other methods. Currently ignored in the other methods.
}
}
\details{
For details on \code{model.filename}, \code{restrictions.filename}, or \code{model.type}, see \code{\link{make.mpt}}.

When calling \code{fit.mpt} with a filename as the \code{model} argument, \code{\link{make.mpt}} is called to create a model object (see \code{\link{bmpt.model-class}}). For models of class \code{bmpt.model} the fast Fortran routine implementing an EM-algorithm is used for fitting and the results are returned in an object of class \code{bmpt}. For models of class \code{mpt.model} the fitting routine that was already implemented in the first version of MPTinR using \code{\link{optim}}'s L-BFGS-B is used (this version of MPTinR allows to use \code{\link{nlminb}} as an alternative to L-BFGS-B) an don object of class \code{mpt} is returned. Note that most of the advanced features of MPTinR, such as the FIA and parametric and non-parametric bootstrapped CIs (all methods currently not implemented), are only available for models that are members of L-BMPT (Purdy & Batchelder, 2009; i.e., of type \code{bmpt.model}).

The index of each datapoint (or the column for matrices) must correspond to the row or number of this response catgeory in the model file.

\code{start.parameters} are used as the initial values when fitting the model.\cr
For models of class \code{bmpt.model} the \code{start.parameters} argument must be either \code{NULL} or a numeric vector of at least length = number of free parameters. If \code{length(start.parameters)} is larger than the number of free parameters it is truncated. The \code{start.parameters} are mapped on the free parameters based on the order (see \code{check(model)[["free.parameters"]]} where model can be either a model object or fitted object).

For models of class \code{mpt.model} multiple cores can be used for fitting via the \code{multicore} argument. Multicore fitting is achieved via the \code{snowfall} package and needs to be initialized via \code{sfInit}. As initialization needs some time, you can either initialize multicore facilities yourself using \code{sfInit()} and setting the \code{sfInit} argument to \code{FALSE} (the default) or let MPTinR initialize multicore facilities by setting the \code{sfInit} argument to \code{TRUE}. The former is recommended as initializing \code{snowfall} takes some time and only needs to be done once if you run \code{fit.mpt} multiple times. If there are any problems with multicore fitting, first try to initialize \code{snowfall} outside MPTinR (e.g., \code{sfInit( parallel=TRUE, cpus=2 )}). If this does not work, the problem is not related to MPTinR but to snowfall (for support and references visit: \url{http://www.imbi.uni-freiburg.de/parallel/}).\cr
When using multicore facilties set argument \code{nCPU} to the desired (and available) number of CPUs.\cr
Note that you should \emph{close} snowfall via \code{sfStop()} after using MPTinR.

}
\value{
An object of class \code{bmpt} or \code{mpt}. See \code{\link{bmpt-class}} for corresponding methods.
}
\note{
Whenever possible try to write your model as a member of L-BMPT (Purdy & Batchelder, 2009) to use the full functionality of \pkg{MPTinR}.
}
\references{
Baldi, P. & Batchelder, W. H. (2003). Bounds on variances of estimators for multinomial processing tree models. \emph{Journal of Mathematical Psychology}, 47, 467-470. 

Byrd, R. H., Lu, P., Nocedal, J., & Zhu, C. (1995). A limited memory algorithm for bound constrained optimization. \emph{SIAM J. Scientific Computing}, 16, 1190-1208.

Knapp, B. R., & Batchelder, W. H. (2004). Representing parametric order constraints in multi-trial applications of multinomial processing tree models. \emph{Journal of Mathematical Psychology}, 48, 215-229.

Moshagen, M. (2010). multiTree: A computer program for the analysis of multinomial processing tree models. \emph{Behavior Research Methods}, 42, 42-54.

Purdy, B. P., & Batchelder, W. H. (2009). A context-free language for binary multinomial processing tree models. \emph{Journal of Mathematical Psychology}, 53, 547-561.

Riefer, D. M., & Batchelder, W. H. (1988). Multinomial modeling and the measurement of cognitive processes. \emph{Psychological Review}, 95, 318-339.

Stahl, C. & Klauer, K. C. (2007). HMMTree: A computer program for latent-class hierarchical multinomial processing tree models. \emph{Behavior Research Methods}, 39, 267- 273.

Wu, H., Myung, J.I., & Batchelder, W.H. (2010a). Minimum description length model selection of multinomial processing tree models. \emph{Psychonomic Bulletin & Review}, 17, 275-286.

Wu, H., Myung, J.I., & Batchelder, W.H. (2010b). On the minimum description length complexity of multinomial processing trees. \emph{Journal of Mathematical Psychology}, 54, 291-303.
}
\author{
Henrik Singmann and David Kellen. 

The Fortran code was written by Karl Christoph Klauer
}

\examples{
\dontrun{
# The first example fits the MPT model presented in Riefer and Batchelder (1988, Figure 1)
# to the data presented in Riefer and Batchelder (1988, Table 1)
# Note that Riefer and Batchelder (1988, pp. 328) did some hypotheses tests, that are not done here.
# Rather, we use each condition (i.e., row in Table 1) as a different individual.


# load the data
data(rb.fig1.data, package = "MPTinR2")

#make model objects, once using the easy format, once using the eqn format.
(model1 <- make.mpt(system.file("extdata", "rb.fig1.model", package = "MPTinR2")))
(model1.eqn <- make.mpt(system.file("extdata", "rb.fig1.model.eqn", package = "MPTinR2")))
#both models are identical:
identical(model1, model1.eqn)

# specify the same model via textConnection
model1.txtCon <- make.mpt(textConnection("p * q * r
p * q * (1-r)
p * (1-q) * r
p * (1-q) * (1-r) + (1-p)"))
identical(model1, model1.txtCon)
# see ?make.mpt for more on how to specify a model and restrictions

# just fit the first "individual":
fit.mpt(model1, rb.fig1.data[1,])

#fit all "individuals":
fit.mpt(model1, rb.fig1.data)

#fit all "individuals" using the .EQN model file:
fit.mpt(model1.eqn, rb.fig1.data)

#fit all "individuals" using the .txtCon model file:
fit.mpt(model1.txtCon, rb.fig1.data)


# The second example fits the MPT model presented in Riefer and Batchelder (1988, Figure 2)
# to the data presented in Riefer and Batchelder (1988, Table 3)
# First, the model without restrictions is fitted: ref.model
# Next, the model with all r set equal is fitted: r.equal
# Then, the model with all c set equal is fitted: c.equal
# Finally, the inferential tests reported by Riefer & Batchelder, (1988, p. 332) are executed.


# get the data
data(rb.fig2.data, package = "MPTinR2")

# make model objects
model2.file <- system.file("extdata", "rb.fig2.model", package = "MPTinR2")
model2 <- make.mpt(model2.file)
model2.r.eq <- make.mpt(model2.file, system.file("extdata", "rb.fig2.r.equal", package = "MPTinR2"))
model2.c.eq <- make.mpt(model2.file, system.file("extdata", "rb.fig2.c.equal", package = "MPTinR2"))

# The full (i.e., unconstrained) model
(ref.model <- fit.mpt(model2, rb.fig2.data))


# All r equal
(r.equal <- fit.mpt(model2.r.eq, rb.fig2.data))

# All c equal
(c.equal <- fit.mpt(model2.c.eq, rb.fig2.data))

# is setting all r equal a good idea?
(g.sq.r.equal <- goodness.of.fit(r.equal)[["G.Squared"]] - goodness.of.fit(ref.model)[["G.Squared"]])
(df.r.equal <- goodness.of.fit(r.equal)[["df.model"]] - goodness.of.fit(ref.model)[["df.model"]])
(p.value.r.equal <- pchisq(g.sq.r.equal, df.r.equal , lower.tail = FALSE))

# is setting all c equal a good idea?
(g.sq.c.equal <- goodness.of.fit(c.equal)[["G.Squared"]] - goodness.of.fit(ref.model)[["G.Squared"]])
(df.c.equal <- goodness.of.fit(c.equal)[["df.model"]] - goodness.of.fit(ref.model)[["df.model"]])
(p.value.c.equal <- pchisq(g.sq.c.equal, df.c.equal , lower.tail = FALSE))

# Example from Broeder & Schuetz (2009)
# We fit the data from the 40 individuals from their Experiment 3
# We fit three different models:
# 1. Their 2HTM model: br.2htm
# 2. A restricted 2HTM model with Dn = Do: br.2htm.res
# 3. A 1HTM model (i.e., Dn = 0): br.1htm
# We fit the models with, as well as without, applied inequality restrictions (see Details)
# that is, for some models (.ineq) we impose: G1 < G2 < G3 < G4 < G5 
# As will be apparent, the inequality restrictions do not hold for all individuals.

data(d.broeder, package = "MPTinR2")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR2")
r.2htm <- system.file("extdata", "broeder.2htm.restr", package = "MPTinR2")
r.1htm <- system.file("extdata", "broeder.1htm.restr", package = "MPTinR2")
i.2htm <- system.file("extdata", "broeder.2htm.ineq", package = "MPTinR2")
ir.2htm <- system.file("extdata", "broeder.2htm.restr.ineq", package = "MPTinR2")
ir.1htm <- system.file("extdata", "broeder.1htm.restr.ineq", package = "MPTinR2")

# fit the original 2HTM
br.2htm <- fit.mpt(m.2htm, d.broeder)
br.2htm.ineq <- fit.mpt(m.2htm, d.broeder, i.2htm)

# do the inequalities hold for all participants?
parameters(br.2htm.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",]
parameters(br.2htm)[["individual"]][,"estimates",]
# See the difference between forced and non-forced inequality restrictions:
round(parameters(br.2htm)[["individual"]][,"estimates",]-
parameters(br.2htm.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",],2)

# The same for the other two models
# The restricted 2HTM
br.2htm.res <- fit.mpt(m.2htm, d.broeder, r.2htm)
br.2htm.res.ineq <- fit.mpt(m.2htm, d.broeder, ir.2htm)
round(parameters(br.2htm.res, sort.alphabetical = TRUE)[["individual"]][,"estimates",]-
parameters(br.2htm.res.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",],2)

# The 1HTM
br.1htm <- fit.mpt(m.2htm, d.broeder, r.1htm)
br.1htm.ineq <- fit.mpt(m.2htm, d.broeder, ir.1htm)
round(parameters(br.1htm, sort.alphabetical = TRUE)[["individual"]][,"estimates",]-
parameters(br.1htm.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",],2)
}
  }
\seealso{
\code{\link{make.mpt}} for the function to create the model objects and \code{\link{bmpt.model-class}} for their methods.

\code{\link{bmpt-class}} for methods of objects returned by this function.

\url{http://www.psychologie.uni-freiburg.de/Members/singmann/R/mptinr} for additional information on model files and restriction files
}
  
\keyword{models}
\keyword{tree}

