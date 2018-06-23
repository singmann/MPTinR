

setGeneric("generate.data", function(model, ...) standardGeneric("generate.data"))

setMethod("generate.data", "mpt.model", function(model, parameter.values, n.per.tree, n = 1, random = TRUE, ...) { 
	# some checks and preparations
	if (is.vector(parameter.values)) parameter.values <- matrix(parameter.values, 1, length(parameter.values), dimnames = list(1, names(parameter.values)))
	n.data <- dim(parameter.values)[1]
	if (is.vector(n.per.tree)) n.per.tree <- matrix(n.per.tree, 1, length(n.per.tree))
	if (n.data > 1){
		if ((dim(n.per.tree)[1] > 1) & (dim(n.per.tree)[1] != n.data)) stop("dim(parameter.values)[1] and dim(n.per.tree)[1] do not match!")
		if (dim(n.per.tree)[1] == 1) n.per.tree <- t(vapply(1:n.data, function(x) n.per.tree[1,, drop = TRUE], n.per.tree[1,, drop = TRUE]))
	}
	# some variables needed
	env.data <- new.env()
	cat.per.tree <- vapply(model.list(model), "length", 0)
	total.n <- sum(n.per.tree)
	# expected values
	expected.proportion <- matrix(NA, n.data, check(model)[["n.categories"]])
	unlist.model <- unlist(model.list(model))
	for (dataset in 1:n.data) {
		for (p in colnames(parameter.values)) assign(p, parameter.values[dataset,p], pos = env.data) 
		expected.proportion[dataset,] <- vapply(unlist.model, eval, envir = env.data, 0)
	}
	# make data from expected values
	if (!random) {
		toReturn <- expected.proportion * n.per.tree
	} else {
		toReturn <- array(NA, dim = c(n, check(model)[["n.categories"]], n.data))
		for (dataset in 1:n.data) {
			category <- 1
			for (tree in cat.per.tree) {
				toReturn[,category:(category + (tree-1)),dataset] <- t(rmultinom(n, n.per.tree[dataset,category], prob = expected.proportion[dataset, category:(category + (tree-1))]))
				category <- category + tree
			}
		}
	}
	toReturn
})

setMethod("generate.data", "mpt", function(model, n = 1, random = TRUE, ...) { 
	n.data <- length(C.matrix(model))
	n.per.tree <- t(vapply(1:n.data, function(n, C, data) C[[n]][,1] * sum(data[n,]), C.matrix(model)[[1]][,1], C = C.matrix(model), data = observed.data(model)))
	callGeneric(model = model(model), parameter.values = estimates(model), n.per.tree = n.per.tree, n = n, random = random, ...)
})
