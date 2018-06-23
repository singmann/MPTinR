
##################
## show methods ##
##################

setMethod("show", "mpt.model", function(object) {
	if (is.null(restrictions(object))) {
		print(c(check(object), restrictions = "none"))
	} else {
	print(c(check(object), restrictions = list(raw.restrictions(object))))
	}
})

setMethod("show", "mpt", function(object) {
	n.digits <- 2
	cat("\nModel info:\n")
	checks <- check(model(object))
	cat(paste("Number of trees: ", checks[["n.trees"]], "\tNumber of categories: ", checks[["n.categories"]], "\n", sep = ""))
	cat(paste("Number of free parameters: ", checks[["n.free.parameters"]], "\tNumber of fixed parameters: ", checks[["n.fixed.parameters"]], "\n", sep = ""))
	cat("Degrees of Freedom:\n")
	print(checks[["df"]])
	cat("\n")
	cat("------------------------------------")
	gof <- goodness.of.fit(object)
	cat("\nGoodness of fit:\n")
	if (multifit(object)) {
		for (c in seq_len(length(gof))) {
			cat(paste(names(gof)[c], ":\n", sep =""))
			if (class(gof[[c]]) == "data.frame") print(gof[[c]], digits = n.digits+2)
			else print(formatC(gof[[c]], digits = n.digits+2, format = "f"), quote = FALSE, right = TRUE)
			cat("\n")
		}
	} else {
		print(formatC(gof, digits = n.digits+2, format = "f"), quote = FALSE, right = TRUE)
		cat("\n")
	}
	cat("------------------------------------")
	ic <- information.criteria(object)
	cat("\nInformation Criteria:\n")
	if (multifit(object)) {
		for (c in seq_len(length(ic))) {
			cat(paste(names(ic)[c], ":\n", sep =""))
			if (class(ic[[c]]) == "data.frame") print(ic[[c]], digits = n.digits)
			else print(formatC(ic[[c]], digits = n.digits, format = "f"), quote = FALSE, right = TRUE)
			cat("\n")
		}
	} else {
		print(formatC(ic, digits = n.digits, format = "f"), quote = FALSE, right = TRUE)
		cat("\n")
	}
	cat("------------------------------------")
	cat("\nParameter Values:\n")
	ps <- parameters(object)
	if (multifit(object)) {
		cat("Mean Values:\n")
		print(ps[["mean"]], digits = n.digits)
		cat("\nAggregated Data:\n")
		print(ps[["aggregated"]], digits = n.digits)
	} else print(ps, digits = n.digits)
	cat("\n")
})


