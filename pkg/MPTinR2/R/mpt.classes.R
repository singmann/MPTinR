
##
## S4 object definitions.
##

# restrictions
setClass("restriction", representation = representation(parameter = "character", "VIRTUAL"))
setClass("fixed.restriction", representation = representation(value = "numeric"), contains = "restriction")
setClass("equality.restriction", representation = representation(value = "character"), contains = "restriction")
setClass("inequality.restriction", representation = representation(exchange.parameter = "list", exchange.inverse = "list", compute.as = "expression"), contains = "restriction")
setClass("restrictions", representation = representation(fixed = "list", equality = "list", inequality = "list", raw = "list"))
setClassUnion("restrictionsOrNull", c("restrictions", "NULL"))

# MPT models
setClass("mpt.model", representation = representation(initial.model = "list", check = "list", restrictions = "restrictionsOrNull", initial.model.data.frame = "data.frame", model.data.frame = "data.frame", model.list = "list"))
setClass("bmpt.model", representation = representation(A = "array", B = "array", lbmpt = "character"), contains = "mpt.model")

# fitted MPT models
setClass("mpt", representation(model = "mpt.model", observed.data = "array",  predicted.data = "array", C.matrix = "list", g2 = "numeric", log.likelihood = "numeric", estimates = "matrix", multifit = "logical", hessian = "list", default.ci = "numeric"))
setClass("bmpt", representation(typeHessian = "character", hfail = "numeric", fia = "list", parametric.ci = "list", nonparametric.ci = "list"), contains = "mpt")

## make.mpt helper classes
setOldClass( c("file", "connection" ) )
setOldClass( c("url", "connection" ) )
setOldClass( c("textConnection", "connection" ) )
setClassUnion("characterOrConnection", c("character", "connection"))
setClassUnion("characterOrNull", c("character", "NULL"))
#setClassUnion("listOrNull", c("list", "NULL"))

