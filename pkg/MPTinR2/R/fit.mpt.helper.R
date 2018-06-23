

.DF.N.get <- function(data, tree){   
	temp <- sapply(tree,length)

	DF <-sum(temp)-length(temp)
	N <- rep(NA,length(temp))
	   
	for (i in 1:length(temp)){
		if (i==1) N[1] <- sum(data[1:temp[1]])
		else N[i] <- sum(data[(sum(temp[1:(i-1)])+1):sum(temp[1:i])])
	}
	return(list(DF,N))
}



.prep.data <- function(data) {
	if (!(is.numeric(data) | is.data.frame(data))) stop("data must be a data.frame or numeric!") 
	if(is.vector(data)) {
		data <- array(data, dim = c(1, length(data)))
	} else
		if(dim(data)[1] == 1) {
			if (is.data.frame(data)) data <- as.matrix(data)
			data <- array(data, dim = c(1,length(data)))
		} else 
			if(is.matrix(data) | is.data.frame(data)) {
				if (is.data.frame(data)) data <- as.matrix(data)
			} else stop("data is neither vector, nor matrix, nor data.frame!")
	
	if (!is.numeric(data)) stop("data must be numeric!")
	data
}


.llk.tree <- function(Q, unlist.tree, data, param.names, length.param.names){
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



####################
## for bmpt only ###
####################

make.C.matrix <- function(n, model, df.n, n_items) {
	n.category <- vector("numeric", check(model)[["n.categories"]])
	for (category in 1:check(model)[["n.categories"]]) {
		n.category[category] <- df.n[[n]][[2]][model.data.frame(model)[which(model.data.frame(model)[,"category"]==category)[1],"tree"]]/n_items[n]
	}
	C <- matrix(0, check(model)[["n.categories"]], check(model)[["max.branches.per.category"]])
	for (category in 1:check(model)[["n.categories"]]) {
		for (branch in 1:check(model)[["branches.per.category"]][category]) {
		C[category,branch] <- n.category[category]
		}
	}
	storage.mode(C) <- "double"
	C
}


.wrapper.fit <- function(data, model, start.params, C.matrix, total.n.parameters) {
	.Fortran("onelc",
	kerncat = as.integer(check(model)[["n.categories"]]),
	kernpar = as.integer(total.n.parameters),
	zweig = as.integer(check(model)[["max.branches.per.category"]]),
	trees = as.integer(check(model)[["n.trees"]]),
	indi = as.integer(1),
	a = A(model),
	b = B(model),
	branch = as.integer(check(model)[["branches.per.category"]]),
	c = C.matrix,
	iDaten = data,
	noc = as.integer(c(rep(TRUE, check(model)[["n.free.parameters"]]), rep(FALSE, check(model)[["n.fixed.parameters"]]))),
	x = as.double(start.params),
	g2 = as.double(0),
	likeli = as.double(0), PACKAGE = "MPTinR2")
}


.fit.bmpt <- function(model, data, start.parameters, n.optim, n.data, C.matrix, fixed.parameters) {
	storage.mode(A(model)) <- "integer"
	storage.mode(B(model)) <- "integer"
	storage.mode(data) <- "integer"
	total.n.parameters <- (check(model)[["n.free.parameters"]]+check(model)[["n.fixed.parameters"]])
	n.free.parameters <- check(model)[["n.free.parameters"]]
	fits <- vector("list", n.data)
	if (!is.null(start.parameters)) {
		if (length(start.parameters) < n.free.parameters) stop("length(start.parameters) < number of free parameters")
		if (length(start.parameters) > n.free.parameters) start.parameters <- start.parameters[1:n.free.parameters]
	}
	if (is.null(start.parameters)) starting.values <- runif(n.free.parameters, 0.1, 0.9)
	else starting.values <- start.parameters
	#browser()
	#dataset <- 1
	for (dataset in seq_len(n.data)) {
		if (n.optim[[1]] == "auto") {
			border <- ifelse(starting.values < 0.5, 1, 0)
			starting.values2 <- ifelse((starting.values > (2/3)) | (starting.values < (1/3)), 1-starting.values, abs(border - runif(n.free.parameters, 0, 0.15)))
			
			fit1 <- .wrapper.fit(data[dataset,,drop = FALSE], model = model, start.params = c(starting.values, fixed.parameters), C.matrix = C.matrix[[dataset]], total.n.parameters = total.n.parameters)
			fit2 <- .wrapper.fit(data[dataset,,drop = FALSE], model = model, start.params = c(starting.values2, fixed.parameters), C.matrix = C.matrix[[dataset]], total.n.parameters = total.n.parameters)
			if (abs(fit1[["g2"]] - fit2[["g2"]]) > 0.01) {
				print(paste("individual:", dataset, "auto fitting"))
				if (fit1[["g2"]] < fit2[["g2"]]) fits[[dataset]] <- fit1
				else fits[[dataset]] <- fit2
				for (run in seq_len(n.optim[[2]])) {
					starting.values <- runif(n.free.parameters, 0.0001, 0.9999)
					fit.tmp <- .wrapper.fit(data[dataset,,drop = FALSE], model = model, start.params = c(starting.values, fixed.parameters), C.matrix = C.matrix[[dataset]], total.n.parameters = total.n.parameters)
					if (fit.tmp[["g2"]] < fits[[dataset]][["g2"]]) fits[[dataset]] <- fit.tmp
				}
			} else {
				if (fit1[["g2"]] < fit2[["g2"]]) fits[[dataset]] <- fit1
				else fits[[dataset]] <- fit2
			}
		} else {
			fits[[dataset]] <- .wrapper.fit(data[dataset,,drop = FALSE], model = model, start.params = c(starting.values, fixed.parameters), C.matrix = C.matrix[[dataset]], total.n.parameters = total.n.parameters)
			for (run in seq_len(n.optim[1]-1)) {
				starting.values <- runif(n.free.parameters, 0.0001, 0.9999)
				fit.tmp <- .wrapper.fit(data[dataset,,drop = FALSE], model = model, start.params = c(starting.values, fixed.parameters), C.matrix = C.matrix[[dataset]], total.n.parameters = total.n.parameters)
				if (fit.tmp[["g2"]] < fits[[dataset]][["g2"]]) fits[[dataset]] <- fit.tmp
			}
		}
	}
	fits
}
