\name{make.mpt}
\docType{methods}
\alias{make.mpt}
\alias{make.mpt-methods}
\alias{make.mpt,characterOrConnection-method}
\alias{make.mpt,characterOrConnection,characterOrConnection-method}
\alias{make.mpt,characterOrConnection,list-method}
\title{Creates a MPT Model Object}
\description{
This generic function in \pkg{MPTinR} creates an object representing a MPT model and returns an object of type \code{bmpt.model} or \code{mpt.model}.
}
\usage{
\S4method{make.mpt}{characterOrConnection}(model, restrictions = NULL, model.type = c("easy", "eqn", "eqn2"), ...)
\S4method{make.mpt}{characterOrConnection,characterOrConnection}(model, restrictions = NULL, model.type = c("easy", "eqn", "eqn2"), ...)
\S4method{make.mpt}{characterOrConnection,list}(model, restrictions, model.type = c("easy", "eqn", "eqn2"), ...)
}
\arguments{
  \item{model}{
A character \code{vector} specifying the location and name of the model file OR a \code{connection} to the model file (can be a \code{\link{textConnection}}, see Details and Example. 
}
  \item{restrictions}{
\code{NULL} or a character \code{vector} specifying the location and name of the restrictions file or a \code{list} containing the restrictions. Default is \code{NULL} which corresponds to no restrictions. See Details
}
  \item{model.type}{
Character vector specifying whether the model file is formatted in the easy way (\code{"easy"}; i.e., each line represents all branches corresponding to a response category) or the traditional EQN syntax (\code{"eqn"} or \code{"eqn2"}; see Details and e.g., Stahl & Klauer, 2007). If \code{model.filename} ends with .eqn or .EQN, \code{model.type} is automatically set to \code{"eqn"}. Default is \code{"easy"}.
}
\item{...}{Currently ignored.}

}
\details{
This functions creates an S4 object representing a MPT or BMPT model (for the distinction see, e.g., Purdy & Batchelder, 2009).

The model can currently be specified in either an external file (using a charcter vector containing the files location) or a connection. In either case it can be in one of the three formats mentioned below. Note, that only the easy format can deal with comments (via #).

The model file is either of the easy format (see \url{http://www.psychologie.uni-freiburg.de/Members/singmann/R/mptinr}) or the "classical" EQN format (see below).\cr
In the easy format (the default) each line corresponds to all branches of a tree (branches are concatenated by a +) that correspond to one of the possible response categories. Trees are separated by at least one empty line. The position of each line must correspond to the position of this response category in the data object (for multi-individudal datasets, matrices or data.frames, to the respective column). \cr
Everything right from a # will be considered a comment and ignored.

In the EQN formats, each line corresponds to a single branch in the model, preceded by two numbers representing tree and response category, respectively, delimited by whitespace. The difference between both types of EQN format (\code{"eqn"} or\code{"eqn2"}) is the way the first line of the model file is treated. If \code{model.file} is set to \code{"eqn"}, \code{MPTinR} will ignore the first line of the model file and will read the rest of the file (as does multiTree; Moshagen, 2010). If \code{model.file} is set to \code{"eqn2"} MPTinR will only read as many lines as indicated in the first line of the EQN model file (as does e.g., HMMTree; Stahl & Klauer, 2007). As default this function expects the easy format, but if the filename ends with .eqn or .EQN and \code{model.type} is \code{"easy"}, \code{model.type} is set to \code{"eqn"}\cr
For further info on the EQN format consult one of the corresponding papers (see e.g., Moshagen, 2010; Stahl & Klauer, 2007). The positions in the data object (number of column for multi-individual fit) must correspond to the category number in the EQN file.\cr

Note that names of parameters in the model file should not start with \code{hank}. Variables with these names can lead to unforeseen problems as variables starting with these letters are used internally to represent inequality restricted parameters.

Restrictions can be defined in either a file (via \code{character} or \code{connection}, as above) or as a list. If it is specified via a list simply make a list containing the restrictions as characters. See also the examples.

\pkg{MPTinR} can handle three different kinds of single or sequential restrictions: inequality restrictions, equality restrictions, and fixed restrictions.\cr
\emph{Inequality restrictions} may be specified using either < or > (e.g., \code{D1 > D2 > D3}). The model is reparametrized to accomodate the inequality restrictions following methods "A" (for <) or "B" (for >) from Knapp & Batchelder (2004). Inequality restrictions can also be combined with fixed restrictions, as in: \code{D1 < D2 < D3 < 0.8}. The reparametrization of inequality restrictions are the first restrictions to be applied to the model in the order given in the restrictions file.\cr
\emph{Equality restrictions} must be specified using = and can also be sequantially (e.g., \code{D1 = D2 = D3}) or contain a fixed restrictions at the end (e.g., \code{D1 = D2 = 0.5}). Equality restrictions are applied (i.e., the model is reparametrized) after all inequality restrictions are applied in the order given in the restrictions file.\cr
\emph{Fixed restrictions} need to be specified using = and are applied directly before fitting the model (e.g., \code{D1 = 0.5}).

Note that \pkg{MPTinR} does not check if the list of restrictions is reasonable and/or if the restricted parameters exist. Restrictions must be specified in a reasonable way by the user! This is especially important for inequality restrictions. For any set of inequality restrictions only the rightmost parameter is preserved, the other parameters are exchanged with temporary parameters. That is, in a restrictions file containig \code{D1 < D2 < D3 < D4} and \code{D1 = 0.5} the latter restrictions is not applied as there exists no parameter D1 after the inequality restrictions are applied (which always takes place first).\cr

Examples of restrictions are (the following could all appear in one restrictions file):\cr
\code{D1 < D2 < D3}\cr
\code{G1 > G2 > G3 > 0.5}\cr
\code{D4 = D3}\cr
\code{B1 = B3 = 0.3333}\cr
\code{X4 = X5 = D3}\cr

A list containing the first three of these restrictions would be:\cr
\code{list("D1 < D2 < D3","G1 > G2 > G3 > 0.5", "D4 = D3")]}

Everyting within a restriction right from a # is considered a comment and will be ignored. Lists cannot contain comments.  
}

\value{
An object of class \code{bmpt.model} or class \code{mpt.model}.

Note that only objects of class \code{bmpt.model} can take the full advantage of \pkg{MPTinR}'s features.
}
\note{
All (model or restriction) files should end with an empty line, otherwise you will se a warning.
}
\references{
Knapp, B. R., & Batchelder, W. H. (2004). Representing parametric order constraints in multi-trial applications of multinomial processing tree models. \emph{Journal of Mathematical Psychology}, 48, 215-229.

Moshagen, M. (2010). multiTree: A computer program for the analysis of multinomial processing tree models. \emph{Behavior Research Methods}, 42, 42-54.

Purdy, B. P., & Batchelder, W. H. (2009). A context-free language for binary multinomial processing tree models. \emph{Journal of Mathematical Psychology}, 53, 547-561.

Stahl, C. & Klauer, K. C. (2007). HMMTree: A computer program for latent-class hierarchical multinomial processing tree models. \emph{Behavior Research Methods}, 39, 267- 273.
}
\author{
Henrik Singmann
}
\note{
\code{\link{fit.mpt}} can call \code{make.mpt} automatically when not called with an model object but a model filename. However, \code{make.mpt} is \emph{not optimized} and may act as a bottleneck. Therefore it is advisable to create a model object when calling \code{fit.mpt} more often with the same model.
}


\seealso{
\code{\link{fit.mpt}} for the fitting routine and \code{\link{bmpt.model-class}} for the classes
}
\examples{

# These examples make model objects from two models from Riefer and Batchelder (1988)
# for fitting and more information regarding the models see ?fit.mpt

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


#get the location of model file 2 and restrictions for that model:
model2.file <- system.file("extdata", "rb.fig2.model", package = "MPTinR2")
model2.restrictions.file <- system.file("extdata", "rb.fig2.r.equal", package = "MPTinR2")

# make model objects via files:
model2 <- make.mpt(model2.file)
model2.r.eq <- make.mpt(model2.file, model2.restrictions.file)

# make model object with text connection and restrictions with text connections:
model2.r.eq.txtCon <- make.mpt(textConnection("
			#trees for condition 0:
			c0 * r0
			(1 - c0) * u0 * u0
			(1 - c0) * u0 * (1 - u0) + (1 - c0) * (1 - u0) * u0
			c0 * (1-r0) + (1-c0) * (1-u0) * (1 - u0)

			u0
			(1 - u0)

			#trees for condition 1:
			c1 * r1
			(1 - c1) * u1 * u1
			(1 - c1) * u1 * (1 - u1) + (1 - c1) * (1 - u1) * u1
			c1 * (1-r1) + (1-c1) * (1-u1) * (1 - u1)

			u1
			(1 - u1)

			#trees for condition 2:
			c2 * r2
			(1 - c2) * u2 * u2
			(1 - c2) * u2 * (1 - u2) + (1 - c2) * (1 - u2) * u2
			c2 * (1-r2) + (1-c2) * (1-u2) * (1 - u2)

			u2
			(1 - u2)

			#trees for condition 3:
			c3 * r3
			(1 - c3) * u3 * u3
			(1 - c3) * u3 * (1 - u3) + (1 - c3) * (1 - u3) * u3
			c3 * (1-r3) + (1-c3) * (1-u3) * (1 - u3)

			u3
			(1 - u3)

			#trees for condition 4:
			c4 * r4
			(1 - c4) * u4 * u4
			(1 - c4) * u4 * (1 - u4) + (1 - c4) * (1 - u4) * u4
			c4 * (1-r4) + (1-c4) * (1-u4) * (1 - u4)

			u4
			(1 - u4)"), textConnection("r0 = r1 = r2 = r3 = r4"))

# make model object with restrictions as list
model2.r.eq.list <- make.mpt(model2.file, list("r0 = r1 = r2 = r3 = r4"))

identical(model2.r.eq, model2.r.eq.txtCon)
identical(model2.r.eq, model2.r.eq.list)

  }

