

setGeneric("fit.mpt", function(model, data, ...) standardGeneric("fit.mpt"))

setMethod("fit.mpt", "character", function(model, data, restrictions.filename = NULL, model.type = c("easy", "eqn", "eqn2"), start.parameters = NULL, ...) {
	model <- make.mpt(model, restrictions.filename, model.type)
		if(is.null(data)) {
		warning("data is NULL, fitting aborted (returning model)")
		return(model)
	}
	data <- .prep.data(data)
	
	if (check(model)[["n.categories"]] != length(data[1,])) stop(paste("Size of data does not correspond to size of model (i.e., model has ", check(model)[["n.categories"]], " categories, data gives ", length(data[1,]), " datapoints).", sep = ""))
	
	if (!is.null(start.parameters)) if (length(start.parameters) != check(model)[["n.free.parameters"]]) stop("length of start.parameters does not correspond to number of free parameters!")
	
	callGeneric(model = model, data = data, ...)
})

setMethod("fit.mpt", "bmpt.model", function(model, data, ci = 95, n.optim = list("auto", 5), start.parameters = NULL, ...) {
	
	data <- .prep.data(data)
	
	if (dim(data)[1] > 1) {
		multifit <- TRUE
		data <- rbind(data, colSums(data))
	} else multifit <- FALSE
	
	
	df.n <- apply(data, 1, .DF.N.get, tree = model.list(model))
	n_items <- sapply(df.n, function (x) sum(x[[2]]))
	dgf <- df.n[[1]][[1]]
	n.data <- dim(data)[1]
	
	v.n.data <- 1:n.data
	
	#tmp.C <- matrix(0, check(model)[["n.categories"]], check(model)[["max.branches.per.category"]])
	C <- lapply(v.n.data, make.C.matrix, model = model, df.n = df.n, n_items = n_items)
	
	total.n.parameters <- (check(model)[["n.free.parameters"]]+check(model)[["n.fixed.parameters"]])
	total.parameter.names <- c(check(model)[["free.parameters"]], check(model)[["fixed.parameters"]])
	
	if (check(model)[["n.fixed.parameters"]] > 0) {
		fixed.parameters <- vector("double", check(model)[["n.fixed.parameters"]])
		for (p in 1:check(model)[["n.fixed.parameters"]]) {
			for (f in 1: length(fixed.restrictions(model))) {
				if (parameter(fixed.restrictions(model)[[f]]) == check(model)[["fixed.parameters"]][p])fixed.parameters[p] <- value(fixed.restrictions(model)[[f]])
			}
		}
	} else {
		fixed.parameters <- NULL
	}
	
	storage.mode(A(model)) <- "integer"
	storage.mode(B(model)) <- "integer"
	storage.mode(data) <- "integer"
	hess.matrix <- array(0, dim = c(check(model)[["n.free.parameters"]], check(model)[["n.free.parameters"]]))
	storage.mode(hess.matrix) <- "double"
	

	
	wrapper.hessian <- function(n, model, data, params, hess.matrix, total.n.parameters) {
		.Fortran("onehess",
		kerncat = as.integer(check(model)[["n.categories"]]),
		kernpar = as.integer(total.n.parameters),
		freepar = as.integer(check(model)[["n.free.parameters"]]),
		zweig = as.integer(check(model)[["max.branches.per.category"]]),
		trees = as.integer(check(model)[["n.trees"]]),
		indi = as.integer(1),
		a = A(model),
		b = B(model),
		branch = as.integer(check(model)[["branches.per.category"]]),
		c = C[[n]],
		iDaten = data[n,,drop = FALSE],
		noc = as.integer(c(rep(TRUE, check(model)[["n.free.parameters"]]), rep(FALSE, check(model)[["n.fixed.parameters"]]))),
		x = as.double(params[n,]),
		hess = hess.matrix,
		ifail = as.integer(0), PACKAGE = "MPTinR2")
	}
	
	fits <- .fit.bmpt(model, data, start.parameters = start.parameters, n.optim = n.optim, n.data = n.data, fixed.parameters = fixed.parameters, C.matrix = C)
	
	
	#fits <- lapply(v.n.data, wrapper.fit, model = model, data = data, start.params = start.params)
	
	g2 <- vapply(fits, "[[",0, i = "g2")
	likeli <- vapply(fits, "[[",0, i = "likeli")
	parameter <- t(vapply(fits, "[[", i = "x", fits[[1]][["x"]]))
	colnames(parameter) <- total.parameter.names
	
	hessianOutput <- lapply(v.n.data, wrapper.hessian, model = model, data = data, params = parameter, hess.matrix = hess.matrix, total.n.parameters = total.n.parameters)
	
	ifail <- vapply(hessianOutput, "[[",0, i = "ifail")
	hessian <- lapply(hessianOutput, "[[", i = "hess")
	typeHessian <- vector("character", n.data)
	
	
	for (c in v.n.data) {
		if (ifail[c] != 0) {
			tmpllk.env <- new.env()
			hessian[[c]] <- tryCatch(numDeriv::hessian(func = .llk.tree, x = parameter[[c]], unlist.tree = unlist(model.list(model)), data = data[c,,drop = FALSE], param.names = total.parameter.names, length.param.names = total.n.parameters), error = function(e) NA)
			typeHessian[c] <- "observed"
		} else typeHessian[c] <- "expected"
	}
	
	# get predicted data
	
	#browser()
	
	predicted.data <- generate.data(model = model, parameter.values = parameter, n.per.tree = t(vapply(v.n.data, function(n, C, data) C[[n]][,1] * sum(data[n,]), rep(0, dim(data)[2]), C=C, data = data)), random = FALSE)
	
	
	# predicted.data <- matrix(NA, n.data, check(model)[["n.categories"]])
	# pred.data <- new.env()
	# unlist.model <- unlist(model.list(model))
	# for (dataset in v.n.data) {
		# for (p in names(parameter[[dataset]])) assign(p, parameter[[dataset]][p], pos = pred.data) 
		# predicted.data[dataset,] <- vapply(unlist.model, eval, envir = pred.data, 0) * (sum(data[dataset,]) * C[[dataset]][,1])
	# }
	
	new("bmpt", model = model, observed.data = data, predicted.data = predicted.data, C.matrix = C, g2 = g2, log.likelihood = likeli, estimates = parameter, multifit = multifit, default.ci = ci, hessian = hessian, hfail = ifail, typeHessian = typeHessian)
	
	#typeHessian = typeHessian, fia = list(), parametric.ci = list(), nonparametric.ci = list()
	
	#round(vapply(fits, "[[",0, i = "g2"), 3)
	
	#hessian[[1]][["hess"]]
	
})



setMethod("fit.mpt", "mpt.model", function(model, data, n.optim = 5, ci = 95, start.parameters = NULL, method = c("L-BFGS-B", "nlminb"), multicore = c("none", "individual", "n.optim"), sfInit = FALSE, nCPU = 2, ...){
	
	if (multicore[1] != "none" & sfInit) {
		require(snowfall)
		sfInit( parallel=TRUE, cpus=nCPU )
	}
	
	llk.tree <- function(Q, unlist.tree, data, param.names, length.param.names){
		Q[Q > 1] <- 1
		Q[Q < 0] <- 0
		#tmpllk.env <- new.env()
		for (i in 1:length.param.names)  assign(param.names[i],Q[i], envir = tmpllk.env)
		
		tree.eval <- vapply(unlist.tree, eval, envir = tmpllk.env, 0)
		if (any(tree.eval < 0)) stop(paste("Model not constructed well. Branch (i.e., line) ", which(tree.eval < 0), " produces probabilities < 0!", sep = ""))
		llk <- data * log(tree.eval)
		llk[data == 0] <- 0
		llk <- sum(llk)
	if (is.na(llk)) llk <- -1e10
	if (llk == -Inf) llk <- -1e10
	return(-llk)
	}


	sat_model <- function(tree, data){
		temp.branch <- sapply(tree,length)
		NNN <- rep(.DF.N.get(data,tree)[[2]], temp.branch)
		temp <- data * log(data/NNN)
		temp[data == 0] <- 0
		llk <- sum(temp)
		return(-llk)
	}
	
	optim.tree <- function(data, tree, llk.tree, param.names, n.params, n.optim, method, start.params)  {
		mpt.optim <- function(x, start.params) {
			if (is.null(start.params)) start.params <- runif(n.params, 0.1, 0.9)
			optim(start.params, llk.tree, unlist.tree = tree, data = data, param.names = param.names, length.param.names = n.params, method = "L-BFGS-B", lower = rep(0, n.params), upper = rep(1, n.params), hessian = TRUE)
		}
		mpt.nlminb <- function(x, start.params) {
			if (is.null(start.params)) start.params <- runif(n.params, 0.1, 0.9)
			nlminb(start.params, objective = llk.tree, lower = rep(0, n.params), upper = rep(1, n.params), unlist.tree = tree, data = data, param.names = param.names, length.param.names = n.params)
		}
		if (method[1] == "L-BFGS-B"){
			if (multicore[1] == "n.optim") {
				out <- sfLapply(1:n.optim, mpt.optim, start.params = start.params)
			} else out <- lapply(1:n.optim, mpt.optim, start.params = start.params)
		}
		if (method[1] == "nlminb"){
			if (multicore[1] == "n.optim") {
				out <- sfLapply(1:n.optim, mpt.nlminb, start.params = start.params)
			} else out <- lapply(1:n.optim, mpt.nlminb, start.params = start.params)
		}
		return(out)
	}
	
	optim.mpt <- function(data, n.data, tree, llk.tree, param.names, n.params, n.optim, start.params, method) {
		
		minim <- vector("list", n.data)
		data.new <- lapply(1:n.data, function(x, data) data[x,], data = data) 
		llks <- array(NA, dim=c(n.data, n.optim))
		
		if (multicore[1] == "individual") {
			 optim.runs <- sfLapply(data.new, optim.tree, tree = unlist(tree), llk.tree = llk.tree, param.names = param.names, n.params = n.params, n.optim = n.optim, start.params = start.params, method = method)
		} else optim.runs <- lapply(data.new, optim.tree, tree = unlist(tree), llk.tree = llk.tree, param.names = param.names, n.params = n.params, n.optim = n.optim, start.params = start.params, method = method)
		
		for (c.outer in 1:n.data) {
			least.llk <- 1e10
			if (method[1] == "L-BFGS-B"){
				for (c in 1: n.optim) {
					llks[c.outer, c] <- -(optim.runs[[c.outer]][[c]][["value"]])
					if (optim.runs[[c.outer]][[c]]["value"] < least.llk) {
						minim[[c.outer]] <- optim.runs[[c.outer]][[c]]
						least.llk <- optim.runs[[c.outer]][[c]][["value"]]
					}
				}
			}
			if (method[1] == "nlminb"){
				for (c in 1: n.optim) {
					llks[c.outer, c] <- -(optim.runs[[c.outer]][[c]][["objective"]])
					if (optim.runs[[c.outer]][[c]]["objective"] < least.llk) {
						minim[[c.outer]] <- optim.runs[[c.outer]][[c]]
						least.llk <- optim.runs[[c.outer]][[c]][["objective"]]
					}
				}
			}
		}
		return(list(minim = minim, optim.runs = optim.runs, llks = llks))
	}
	.apply.old.restriction <- function(tree, search, exchange) {
		safeDeparse <- function(expr){
			ret <- paste(deparse(expr), collapse="")
			#rm whitespace
			ret <- gsub("[[:space:]][[:space:]]+", " ", ret)
			gsub("^expression\\(", "", gsub("[[:space:]][[:space:]]", " ", gsub("\\)$", "", ret)))
		}
		for (c1 in 1:length(tree)){
			for (c2 in 1:length(tree[[c1]])) {
				tree[[c1]][c2] <- parse(text = gsub(paste("\\<",search, "\\>", sep =""), exchange, safeDeparse(tree[[c1]][c2])))[1]
			}
		}
		return(tree)
	}
	
	###############################################################################################
	### above functions for MPTinR, below the code that calls them ################################
	###############################################################################################
	
	
	data <- .prep.data(data)
	
	if (dim(data)[1] > 1) {
		multifit <- TRUE
		data <- rbind(data, colSums(data))
	} else multifit <- FALSE
	
	
	
	df.n <- apply(data, 1, .DF.N.get, tree = model.list(model))
	n_items <- sapply(df.n, function (x) sum(x[[2]]))
	dgf <- df.n[[1]][[1]]
	n.data <- dim(data)[1]
	C <- lapply(1:n.data, make.C.matrix, model = model, df.n = df.n, n_items = n_items)
	
	data.smaller.5 <- t(apply(data, 1, function(x) x < 5))
	if (any(data.smaller.5)) warning(paste("Categories have n < 5! Do NOT trust these CIs. Dataset:", paste((1:n.data)[apply(data.smaller.5, 1, any)], collapse = " "), sep = ""))
	
	used.model <- model.list(model)
	
	#browser()
	
	if (!(is.null(restrictions(model)))){
		if (!is.null(fixed.restrictions(model))) {
			for (restriction in fixed.restrictions(model)) {
				used.model <- .apply.old.restriction(used.model, parameter(restriction), value(restriction))
			}
		}
	}
	if (check(model)[["n.fixed.parameters"]] > 0) {
		fixed.parameters <- vector("double", check(model)[["n.fixed.parameters"]])
		for (p in 1:check(model)[["n.fixed.parameters"]]) {
			for (f in 1: length(fixed.restrictions(model))) {
				if (parameter(fixed.restrictions(model)[[f]]) == check(model)[["fixed.parameters"]][p])fixed.parameters[p] <- value(fixed.restrictions(model)[[f]])
			}
		}
	} else {
		fixed.parameters <- NULL
	}
	
	#browser()
	
	tmpllk.env <- new.env()
	t0 <- Sys.time()
	print(paste("Model fitting begins at ", t0, sep = ""))
	flush.console()
	res.optim <- optim.mpt(data = data, n.data = n.data, tree = unlist(used.model), llk.tree = llk.tree, param.names= check(model)[["free.parameters"]], n.params = check(model)[["n.free.parameters"]], n.optim = n.optim, start.params = start.parameters, method = method)
	t1 <- Sys.time()
	print(paste("Model fitting stopped at ", t1, sep = ""))
	print(t1-t0)
	
	
	parameter <- t(vapply(res.optim$minim, function(x) c(x[["par"]], fixed.parameters), c(res.optim[["minim"]][[1]][["par"]], fixed.parameters)))
	colnames(parameter) <- c(check(model)[["free.parameters"]], check(model)[["fixed.parameters"]])
	
	if (method[1] == "L-BFGS-B") {
		likeli <- vapply(res.optim$minim, "[[", 0, i = "value")
		hessian <- lapply(res.optim$minim, "[[", i = "hessian")
	}
	if (method[1] == "nlminb") {
		likeli <- vapply(res.optim$minim, "[[", 0, i = "objective")
		hessian <- lapply(res.optim$minim, function(x) numDeriv::hessian(func = llk.tree, x = x$par, unlist.tree = unlist(used.model), data = data, param.names = check(model)[["free.parameters"]], length.param.names = check(model)[["n.free.parameters"]]))
	}
	
	g2 <- sapply(1:n.data, function(x, data, Log.Likelihood) as.numeric(2*(Log.Likelihood[x]-sat_model(used.model, data[x,]))), data = data, Log.Likelihood = likeli)
	likeli <- 2*likeli
	
	
	if (multifit) {
		for (c.n in 1:n.data) {
			if (method[1] == "L-BFGS-B") {
				if (res.optim$minim[[c.n]][["counts"]][1] < 10) warning(paste("Number of iterations run by the optimization routine for individual ", c.n, " is low (i.e., < 10) indicating local minima. Try n.optim >= 5.", sep = ""))
			}
			if (res.optim$minim[[c.n]][["convergence"]] != 0) warning(paste("Optimization routine for individual ", c.n, " did not converge succesfully. Error code: ", res.optim$minim[[c.n]][["convergence"]], sep =""))
		}
	} else {
		if (method[1] == "L-BFGS-B") {
			if (res.optim$minim[[1]][["counts"]][1] < 10) warning("Number of iterations run by the optimization routine is low (i.e., < 10) indicating local minima. Try n.optim >= 5.")
		}
		if (res.optim$minim[[1]][["convergence"]] != 0) warning(paste("Optimization routine did not converge succesfully. Error code is, ", minim[[1]][["convergence"]], sep =""))
	}
	
	predicted.data <- generate.data(model = model, parameter.values = parameter, n.per.tree = t(vapply(1:n.data, function(n, C, data) C[[n]][,1] * sum(data[n,]), rep(0, dim(data)[2]), C=C, data = data)), random = FALSE)
	
	if (multicore[1] != "none" & sfInit) sfStop()
	
	new("mpt", model = model, observed.data = data, predicted.data = predicted.data, C.matrix = C, g2 = g2, log.likelihood = likeli, estimates = parameter, multifit = multifit, default.ci = ci, hessian = hessian)
	
})





