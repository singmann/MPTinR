
data(rb.fig1.data, package = "MPTinR")

rb.fig1.model <- list(
		expression(p * q * r),
		expression(p * q * (1-r)),
		expression(p * (1-q) * r),
		expression(p * (1-q) * (1-r) + (1-p))
		)
		
	
llk.tree <- function(Q, unlist.tree, data, param.names, length.param.names){
	Q[Q > 1] <- 1
	Q[Q < 0] <- 0
	# tmpllk.env <- new.env()
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

model <- rb.fig1.model
data <- rb.fig1.data[1,]

tmpllk.env <- new.env()
param.names <- unique(unlist(lapply(model, all.vars)))
start.params <- runif(length(param.names))

nlminb(start.params, llk.tree, unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names), lower = 0, upper = 1)


xx <- as.character(unlist(rb.fig1.model))
yy <- lapply(xx, function(x) paste("log(", x, ")", sep =""))

for (c1 in seq_len(length(yy))) {
	yy[[c1]] <- paste(data[c1], "*", yy[[c1]])
}

yy1 <- do.call("paste", args = list(yy, "+"))
yy2 <- do.call("paste", as.list(yy1))
yy3 <- substr(yy2, 0, nchar(yy2) -2)
yy4 <- parse(text = yy3)
likelihood.function <- yy4

llk.tree.gradient <- function(Q, unlist.tree, data, param.names, length.param.names){
	Q[Q > 1] <- 1
	Q[Q < 0] <- 0
	# tmpllk.env <- new.env()
	for (i in 1:length.param.names)  assign(param.names[i],Q[i], envir = tmpllk.env)
	llk.functions <- vector("list", length.param.names)
	for (param in seq_along(param.names)) {
		llk.functions[[param]] <- D(likelihood.function, param.names[param])
	}
	tree.eval <- vapply(llk.functions, eval, 0, envir = tmpllk.env)
	tree.eval[is.na(tree.eval)] <- -1e10
	tree.eval[tree.eval == -Inf] <- -1e10
	return(-tree.eval)
}

(t3 <- nlminb(start.params, llk.tree, unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names), lower = 0, upper = 1, gradient = llk.tree.gradient))

llk.tree.gradient(runif(3), unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names))

llk.tree.gradient(c(1, 0.494, .3), unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names))

llk.tree.gradient(c(1, 0.489, .492), unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names))


llk.tree.hessian <- function(Q, unlist.tree, data, param.names, length.param.names){
	Q[Q > 1] <- 1
	Q[Q < 0] <- 0
	# tmpllk.env <- new.env()
	for (i in 1:length.param.names)  assign(param.names[i],Q[i], envir = tmpllk.env)
	llk.functions <- vector("list", length.param.names)
	for (param in seq_along(param.names)) {
		llk.functions[[param]] <- D(likelihood.function, param.names[param])
	}
	llk.functions.second <- vector("list", length.param.names * length.param.names)
	for (llk.funct.outer in seq_along(llk.functions)){
		for (llk.funct.inner in seq_along(llk.functions)){
			llk.functions.second[[((llk.funct.outer - 1) * length.param.names) + llk.funct.inner]] <- D(llk.functions[[llk.funct.outer]], param.names[llk.funct.inner])
		}
	}
	tree.eval <- vapply(llk.functions.second, eval, 0, envir = tmpllk.env)
	tree.eval[is.na(tree.eval)] <- -1e10
	tree.eval[tree.eval == -Inf] <- -1e10
	matrix(-tree.eval, length.param.names)
}


(t4 <- nlminb(start.params, llk.tree, unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names), lower = 0, upper = 1, gradient = llk.tree.gradient, hessian = llk.tree.hessian))

h <- llk.tree.hessian(c(1, 0.494, .3), unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names))

var.params <- diag(solve(h))
estimates <- t3$par 
ci <- 95
confidence.interval <- qnorm(1-((100-ci)/2)/100)*sqrt(var.params)
upper.conf <- estimates + confidence.interval
lower.conf <- estimates - confidence.interval



system.time(replicate(1000, nlminb(start.params, llk.tree, unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names), lower = 0, upper = 1)))

system.time(replicate(1000, nlminb(start.params, llk.tree, unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names), lower = 0, upper = 1, gradient = llk.tree.gradient)))

system.time(replicate(1000, nlminb(start.params, llk.tree, unlist.tree = model, data = data, param.names = param.names, length.param.names = length(param.names), lower = 0, upper = 1, gradient = llk.tree.gradient, hessian = llk.tree.hessian)))
