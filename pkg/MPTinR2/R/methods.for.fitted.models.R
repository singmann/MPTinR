

###########
### MPT ###
###########

if(!isGeneric("check")){
	if (is.function("check"))
		fun <- check
	else fun <- function(object) standardGeneric("check")
	setGeneric("check", fun)
}
setMethod("check", "mpt", function(object) check(MPTinR2:::model(object)))

if(!isGeneric("hessian.mpt")){
	if (is.function("hessian.mpt"))
		fun <- hessian.mpt
	else fun <- function(object) standardGeneric("hessian.mpt")
	setGeneric("hessian.mpt", fun)
}
setMethod("hessian.mpt", "mpt", function(object) object@hessian)


if(!isGeneric("C.matrix")){
	if (is.function("C.matrix"))
		fun <- C.matrix
	else fun <- function(object) standardGeneric("C.matrix")
	setGeneric("C.matrix", fun)
}
setMethod("C.matrix", "mpt", function(object) object@C.matrix)

if(!isGeneric("observed.data")){
	if (is.function("observed.data"))
		fun <- data
	else fun <- function(object) standardGeneric("observed.data")
	setGeneric("observed.data", fun)
}
setMethod("observed.data", "mpt", function(object) object@observed.data)


if(!isGeneric("predicted.data")){
	if (is.function("predicted.data"))
		fun <- data
	else fun <- function(object) standardGeneric("predicted.data")
	setGeneric("predicted.data", fun)
}
setMethod("predicted.data", "mpt", function(object) object@predicted.data)


if(!isGeneric("logLikelihood")){
	if (is.function("logLikelihood"))
		fun <- data
	else fun <- function(object) standardGeneric("logLikelihood")
	setGeneric("logLikelihood", fun)
}
setMethod("logLikelihood", "mpt", function(object) object@log.likelihood)


if(!isGeneric("g.squared")){
	if (is.function("g.squared"))
		fun <- data
	else fun <- function(object) standardGeneric("g.squared")
	setGeneric("g.squared", fun)
}
setMethod("g.squared", "mpt", function(object) object@g2)


if(!isGeneric("multifit")){
	if (is.function("multifit"))
		fun <- multifit
	else fun <- function(object) standardGeneric("multifit")
	setGeneric("multifit", fun)
}
setMethod("multifit", "mpt", function(object) object@multifit)


if(!isGeneric("estimates")){
	if (is.function("estimates"))
		fun <- estimates
	else fun <- function(object) standardGeneric("estimates")
	setGeneric("estimates", fun)
}
setMethod("estimates", "mpt", function(object) object@estimates)


if(!isGeneric("default.ci")){
	if (is.function("default.ci"))
		fun <- default.ci
	else fun <- function(object) standardGeneric("default.ci")
	setGeneric("default.ci", fun)
}
setMethod("default.ci", "mpt", function(object) object@default.ci)
setGeneric("default.ci<-", function(x, value) standardGeneric("default.ci<-"))
setReplaceMethod("default.ci", "mpt", function(x, value) {
	if (!is.numeric(value)) stop("default.ci must be numeric")
	x@default.ci <- value
	x
})

if(!isGeneric("model")){
	if (is.function("model"))
		fun <- model
	else fun <- function(object) standardGeneric("model")
	setGeneric("model", fun)
}
setMethod("model", "mpt", function(object) object@model)


if(!isGeneric("parameters")){
	if (is.function("parameters"))
		fun <- parameters
	else fun <- function(object, ...) standardGeneric("parameters")
	setGeneric("parameters", fun)
}

setMethod("parameters", "mpt", function(object,  ci = NULL, show.equality = FALSE, show.fixed = TRUE, sort.alphabetical = FALSE, ...) {
	
	if (is.null(ci)) ci <- default.ci(object)
	
	#browser()
	
	total.n.parameters <- (check(model(object))[["n.free.parameters"]]+check(model(object))[["n.fixed.parameters"]])
	total.parameter.names <- c(check(model(object))[["free.parameters"]], check(model(object))[["fixed.parameters"]])
	original.parameters <- check(model(object))[["original.parameters"]]
	free.parameters <- check(model(object))[["free.parameters"]]
	n.data <- dim(observed.data(object))[1]
	v.n.data <- 1:n.data
	estimates <- estimates(object)
	
	
	#browser()
	var.params <- vapply(hessian.mpt(object), function(x) tryCatch(diag(solve(x)), error = function(e) rep(NA, check(model(object))[["n.free.parameters"]])), rep(0, check(model(object))[["n.free.parameters"]]))
	rownames(var.params) <- check(model(object))[["free.parameters"]]
	estimates.m <- t(estimates(object))
	confidence.interval <- qnorm(1-((100-ci)/2)/100)*sqrt(var.params)
	upper.conf <- estimates.m[check(model(object))[["free.parameters"]],] + confidence.interval
	lower.conf <- estimates.m[check(model(object))[["free.parameters"]],] - confidence.interval
	no.ci <- which(is.na(lower.conf[1,]))
	
	if (is.null(restrictions(model(object)))) {
		if (multifit(object)) {
			individual <- array(c(as.vector(estimates.m[,-n.data]), as.vector(lower.conf[,-n.data]), as.vector(upper.conf[,-n.data])), dim = c(total.n.parameters, n.data-1, 3))
			individual <- aperm(individual, c(1,3,2))
			dimnames(individual) <- list(total.parameter.names, c("estimates", "lower.ci", "upper.ci"), paste("dataset:", 1:(n.data-1)))
			mean.df = data.frame(estimates = apply(individual, c(1,2), mean)[,1], lower.conf = NA, upper.conf = NA, row.names = total.parameter.names)
		}
		df.last <- data.frame(estimates = estimates.m[,n.data], lower.ci = lower.conf[,n.data], upper.ci = upper.conf[,n.data], row.names = total.parameter.names)
	} else {
		if (length(inequality.restrictions(model(object))) > 0) {
			totally.free.parameters <- free.parameters[free.parameters %in% original.parameters]
			inequality.parameters <- vapply(inequality.restrictions(model(object)), parameter, "")
			display.parameters <- c(free.parameters[free.parameters %in% original.parameters], inequality.parameters)
		} else {
			totally.free.parameters <- free.parameters
			inequality.parameters <- NULL
			display.parameters <- free.parameters
		}
		if (length(fixed.restrictions(model(object))) > 0) {
			fixed.parameters <- vapply(fixed.restrictions(model(object)), parameter, "")
			if (show.fixed) display.parameters <- c(display.parameters, fixed.parameters)
		} else {
			fixed.parameters <- NULL
		}
		if (length(equality.restrictions(model(object))) > 0) {
			equality.parameters <- vapply(equality.restrictions(model(object)), parameter, "")
			if (show.equality) display.parameters <- c(display.parameters, equality.parameters)
		} else {
			equality.parameters <- NULL
		}
		display.parameters <- display.parameters[display.parameters %in% original.parameters]
		values.for.matrix <- vector("numeric", n.data*4*length(display.parameters))
		tmp.param <- new.env()
		for (dataset in v.n.data) {
			new.estimates <- matrix(NA, length(display.parameters), 4)
			rownames(new.estimates) <- display.parameters
			rm(list = ls(pos = tmp.param), pos = tmp.param)
			for (parameter in 1:total.n.parameters) assign(colnames(estimates)[parameter], estimates[dataset, parameter], envir = tmp.param)
			if (!is.null(inequality.parameters)) {
				for (inequality in rev(inequality.restrictions(model(object)))) assign(parameter(inequality), eval(compute.as(inequality), envir = tmp.param), envir = tmp.param)
			}
			for (parameter in totally.free.parameters) new.estimates[parameter,] <- c(get(parameter, pos = tmp.param), lower.conf[parameter, dataset], upper.conf[parameter, dataset], NA)
			for (parameter in inequality.parameters) {
				new.estimates[parameter,1] <- get(parameter, pos = tmp.param)
				new.estimates[parameter,2:3] <- NA
				new.estimates[parameter,4] <- 3
			}
			if (show.fixed) if (!is.null(fixed.parameters)) for (parameter in rev(fixed.restrictions(model(object)))) if (parameter(parameter) %in% original.parameters) new.estimates[parameter(parameter),] <- c(value(parameter), NA, NA, 1)
			if (show.equality) {
				if (!is.null(equality.parameters)){
					for (parameter in rev(equality.restrictions(model(object)))) new.estimates[parameter(parameter),] <- c(new.estimates[value(parameter),1:3],2)
				}
			}
			if (sort.alphabetical) new.estimates <- new.estimates[order(row.names(new.estimates)),]
			values.for.matrix[((dataset-1)*4*length(display.parameters)+1):((dataset)*4*length(display.parameters))] <- as.vector(new.estimates)
		}
		if (sort.alphabetical) rownames.parameters <- sort(display.parameters)
		else rownames.parameters <- display.parameters
		full.array <- array(values.for.matrix, dim = c(length(display.parameters), 4, n.data))
		if (multifit(object)) {
			individual <- full.array[,,-n.data]
			dimnames(individual) <- list(rownames.parameters, c("estimates", "lower.ci", "upper.ci", "restricted"), paste("dataset:", 1:(n.data-1)))
			mean.df = data.frame(estimates = apply(individual, c(1,2), mean)[,1], lower.conf = NA, upper.conf = NA, restricted = apply(individual, c(1,2), mean)[,4], row.names = rownames.parameters)
		}
		
		df.last <- as.data.frame(full.array[,,n.data])
		colnames(df.last) <- c("estimates", "lower.ci", "upper.ci", "restricted")
		rownames(df.last) <- rownames.parameters
		df.last[,"restricted"] <- as.character(factor(df.last[,"restricted"], c(NA, 1:3), labels = c("", "fixed", "equality", "inequality"), exclude = 99))
	}
	#recover()
	if (multifit(object)) return(list(individual = individual, mean = mean.df, aggregated = df.last))
	else return(df.last)
	
})



if(!isGeneric("goodness.of.fit")){
	if (is.function("goodness.of.fit"))
		fun <- goodness.of.fit
	else fun <- function(object, ...) standardGeneric("goodness.of.fit")
	setGeneric("goodness.of.fit", fun)
}
setMethod("goodness.of.fit", "mpt", function(object, ...) {
	n.data <- dim(observed.data(object))[1]
	if (multifit(object)) {
		individual <- data.frame(m2.Log.Likelihood = logLikelihood(object)[-n.data], G.Squared = g.squared(object)[-n.data], df = check(model(object))[["df"]]["model"], p = pchisq(g.squared(object)[-n.data], check(model(object))[["df"]]["model"],lower.tail=FALSE), row.names = NULL)
		sum.tmp <- colSums(individual)
		sum <- c(sum.tmp[1:3], p = pchisq(sum.tmp[2],sum.tmp[3],lower.tail = FALSE))
		names(sum)[4] <- "p"
	} 
	aggregated <- c(m2.Log.Likelihood = logLikelihood(object)[n.data], G.Squared = g.squared(object)[n.data], df = check(model(object))[["df"]]["model"], p = pchisq(g.squared(object)[n.data], check(model(object))[["df"]]["model"],lower.tail=FALSE))
	
	if (multifit(object)) return(list(individual = individual, sum = sum, aggregated = aggregated))
	else return(aggregated)	
})

if(!isGeneric("information.criteria")){
	if (is.function("information.criteria"))
		fun <- information.criteria
	else fun <- function(object, ...) standardGeneric("information.criteria")
	setGeneric("information.criteria", fun)
}
setMethod("information.criteria", "mpt", function(object, ...) {
	#browser()
	n.data <- dim(observed.data(object))[1]
	AIC <- g.squared(object) + 2*check(model(object))[["n.free.parameters"]]
	BIC <- g.squared(object) + check(model(object))[["n.free.parameters"]]*log(rowSums(observed.data(object)))
	if (multifit(object)) {
		individual <- data.frame(AIC = AIC[-n.data], BIC = BIC[-n.data], row.names = NULL)
		sum <- colSums(individual)
	} 
	aggregated <- c(AIC = AIC[n.data], BIC = BIC[n.data])
	
	if (multifit(object)) return(list(individual = individual, sum = sum, aggregated = aggregated))
	else return(aggregated)	
})


###############
## BMPT only ##
###############


if(!isGeneric("hfail")){
	if (is.function("hfail"))
		fun <- hfail
	else fun <- function(object) standardGeneric("hfail")
	setGeneric("hfail", fun)
}
setMethod("hfail", "bmpt", function(object) object@hfail)

if(!isGeneric("typeHessian")){
	if (is.function("typeHessian"))
		fun <- typeHessian
	else fun <- function(object) standardGeneric("typeHessian")
	setGeneric("typeHessian", fun)
}
setMethod("typeHessian", "bmpt", function(object) object@typeHessian)


setMethod("parameters", "bmpt", function(object, type = c("nonparametric", "parametric", "fisher"), ci = NULL, show.equality = FALSE, show.fixed = TRUE, sort.alphabetical = FALSE, ...) {
	
	if (all(type == c("nonparametric", "parametric", "fisher"))) {
		if (length(object@nonparametric.ci) == 0) {
			if (length(object@parametric.ci) == 0) {
				type <- "fisher"
			} else type <- "parametric"
		} else type <- "nonparametric"
	}
	
	if (is.null(ci)) ci <- default.ci(object)
	
	#browser()
	
	total.n.parameters <- (check(model(object))[["n.free.parameters"]]+check(model(object))[["n.fixed.parameters"]])
	total.parameter.names <- c(check(model(object))[["free.parameters"]], check(model(object))[["fixed.parameters"]])
	original.parameters <- check(model(object))[["original.parameters"]]
	free.parameters <- check(model(object))[["free.parameters"]]
	n.data <- dim(observed.data(object))[1]
	v.n.data <- 1:n.data
	estimates <- estimates(object)
	
	
	
	if (type[1] == "fisher") {
		#browser()
		var.params <- vapply(hessian.mpt(object), function(x) tryCatch(diag(solve(x)), error = function(e) rep(NA, check(model(object))[["n.free.parameters"]])), rep(0, check(model(object))[["n.free.parameters"]]))
		rownames(var.params) <- check(model(object))[["free.parameters"]]
		estimates.m <- t(estimates(object))
		confidence.interval <- qnorm(1-((100-ci)/2)/100)*sqrt(var.params)
		upper.conf <- estimates.m[check(model(object))[["free.parameters"]],] + confidence.interval
		lower.conf <- estimates.m[check(model(object))[["free.parameters"]],] - confidence.interval
		no.ci <- which(is.na(lower.conf[1,]))
		
		if (any(hfail(object) != 0)) {
			if (1 %in% hfail(object)) {
				writeLines(strwrap(paste("Warning for dataset(s) ", paste(which(hfail(object) == 1), collapse = " ") ,": The Fisher information is very ill-conditioned; confidence intervals and standard errors of parameter estimates may be inaccurate; values and degrees of freedom for goodness-of-fit statistics M2 and S2 may be compromised. Possible causes include:", sep = "")))
				writeLines(strwrap("- The core model is not identified (fatal);"))
				writeLines(strwrap("- There are too few observations per person, the multinomial model is not identified (try a model with fewer core-model parameters);"))
				writeLines(strwrap("Parameter values fall onto, or close to, the boundary values of the parameter space (0 or 1). In this case, try replacing parameter values (and constants) very close to zero by less extreme values, e.g., by 0.0001, and parameter values (and constants) very close to one by less extreme values, e.g., by 0.9999."))
				cat("\n")
			}
			if (2 %in% hfail(object)) {
				writeLines(strwrap(paste("Warning for dataset(s) ", paste(which(hfail(object) == 2), collapse = " ") ,": The Fisher information is not positive definite: Possible causes include:", sep = "")))
				writeLines(strwrap("- The core model is not identified (fatal);"))
				writeLines(strwrap("- There are too few observations per person, the multinomial model is not identified (try a model with fewer core-model parameters);"))
				writeLines(strwrap("Parameter values fall onto, or close to, the boundary values of the parameter space (0 or 1). In this case, try replacing parameter values (and constants) very close to zero by less extreme values, e.g., by 0.0001, and parameter values (and constants) very close to one by less extreme values, e.g., by 0.9999."))
				cat("\n")
			}
			if (3 %in% hfail(object)) {
				writeLines(strwrap(paste("Warning for dataset(s) ", paste(which(hfail(object) == 3), collapse = " ") ,": One of the expected aggregated cell counts is zero.", sep = "")))
				cat("\n")
			}
			if (any(typeHessian(object)[-no.ci] != "expected")) {
				observed.all <- which(typeHessian(object) != "expected")
				writeLines(strwrap(paste("Warning for dataset(s) ", paste(observed.all[!(observed.all %in% no.ci)], collapse = " "), ": CIs are based on OBSERVED Fisher information (not expected)!", sep = "")))
				cat("\n")
			}
		}
		return(callNextMethod(object, ci = ci, show.equality = show.equality, show.fixed = show.fixed, sort.alphabetical = sort.alphabetical, ...))
	}
	
	if (multifit(object)) return(list(individual = individual, mean = mean.df, aggregated = df.last))
	else return(df.last)
	
})

