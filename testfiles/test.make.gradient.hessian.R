




# Examples:
model <- .get.mpt.model(system.file("extdata", "5points.2htm.model", package = "MPTinR2"), "easy")

param.names <- .find.MPT.params(model)
length.param.names <- length(param.names)

# example llk

model.llk <- .make.llk.function(model,param.names,length.param.names)

# example gradient

.make.llk.gradient(model.llk,param.names,length.param.names)

# example hessian

.make.llk.hessian(model.llk,param.names,length.param.names)


llk.hessian.funct(c(1,0.49, 0.3), unlist(tree), data, param.names, length.param.names, lower.bound, upper.bound, llk.gradient, llk.hessian)
