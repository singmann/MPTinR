

.read.mpt <- function(model.filename, model.type) {
	if (grepl("\\.eqn$", model.filename) || grepl("\\.EQN$", model.filename)) model.type <- "eqn"
	if (model.type[1] == "eqn") {
		raw.model <- .read.EQN.model(model.filename)
	} else if (model.type[1] == "eqn2") {
		raw.model <- .read.EQN.model.2(model.filename)
	} else raw.model <- .read.MPT.model(model.filename)
	raw.model
}

.read.MPT.model <- function(model.filename) {
	whole <- readLines(model.filename)
	model <- vector("list", length(whole))
	c2 <- 1
	c3 <- 1
	s.flag <- FALSE
	for (c1 in 1:length(whole)) {
		if (!(grepl("^[[:space:]]*$", whole[c1]))) {
			if (grepl("^[[:space:]]*#", whole[c1])) next
			whole[c1] <- gsub("#.*", "", whole[c1])
			s.flag <- TRUE
			model[[c2]][c3] <- parse(text = whole[c1])[1]
			c3 <- c3 + 1
			fin <- c2
		}
		else {
			if (s.flag == TRUE) c2 <- c2 + 1
			c3 <- 1
			s.flag <- FALSE
		}
	}
	return (model[1:fin])
}


# this version of a function to read EQN files behaves like MultiTree, that is, it ignores the content of the first line and reads 
# all remaining lines!

.read.EQN.model <- function(model.filename) {
	parse.eqn <- function(x){
		branches <- unique(x[,2])
		l.tree <- length(branches)
		tree <- vector('expression', l.tree)
		for (branch in 1:l.tree) {
			tree[branch] <- parse(text = paste(x[x$V2 == branches[branch],"V3"], collapse = " + "))
		}
		tree
	}
	tmp.in <- read.table(model.filename, skip = 1, stringsAsFactors = FALSE)
	tmp.ordered <- tmp.in[order(tmp.in$V1),]
	tmp.spl <- split(tmp.ordered, factor(tmp.ordered$V1))
	tmp.spl <- lapply(tmp.spl, function(d.f) d.f[order(d.f[,2]),])
	model <- lapply(tmp.spl, parse.eqn)
	names(model) <- NULL
	model
}



# this version of a function to read EQN files behaves like HMMtree and GPT, that is, it only read the number of lines that are 
# indicated in the first line

# NOTE. This function is currently not used.

.read.EQN.model.2 <- function(model.filename) {
	parse.eqn <- function(x){
		branches <- unique(x[,2])
		l.tree <- length(branches)
		tree <- vector('expression', l.tree)
		for (branch in 1:l.tree) {
			tree[branch] <- parse(text = paste(x[x$V2 == branches[branch],"V3"], collapse = " + "))
		}
		tree
	}
	n.lines <- read.table(model.filename, nrows = 1)[1,1]
	tmp.in <- read.table(model.filename, skip = 1, stringsAsFactors = FALSE, nrows = n.lines)
	tmp.ordered <- tmp.in[order(tmp.in$V1),]
	tmp.spl <- split(tmp.ordered, factor(tmp.ordered$V1))
	tmp.spl <- lapply(tmp.spl, function(d.f) d.f[order(d.f[,2]),])
	model <- lapply(tmp.spl, parse.eqn)
	names(model) <- NULL
	model
}




#rules for restriction files:
# 1. Use your brain!
# 2. At first all inequality restrictions are applied.
# 3. Then all equality restrictions.
# 4. Finally all fixed parameters are handled.
# If your set of restrictions does not make sens given this procedure, change your set of restrictions.

.read.MPT.restrictions.file <- function(filename) {
	# filename can be a connection, there should be an see examples
	whole <- readLines(filename)
	model <- vector("list", length(whole))
	c2 <- 1
	for (c1 in 1:length(whole)) {
		if (!(grepl("^[[:space:]]*$", whole[c1]))) {
			if (grepl("^[[:space:]]*#", whole[c1])) next
			whole[c1] <- gsub("#.*", "", whole[c1])
			model[[c2]] <- whole[c1]
			fin <- c2
			c2 <- c2 + 1
		}
	}
	if(!exists("fin")) return(NULL)
	.read.MPT.restrictions(model[1:fin])
}

.read.MPT.restrictions <- function(tmp.restrictions) {
	#min.restriction <- c(0, 0.001) 
	#max.restriction <- 0.99999
	
	if (!is.list(tmp.restrictions)) stop("restrictions must be a list")
	
	if (!all(vapply(tmp.restrictions, class, "") == "character")) stop("The restrictions can only be of class character")
	
	no.white.restrictions <- lapply(tmp.restrictions, gsub, pattern = " ", replacement = "")
	if (sum(grepl("[/\\+\\*\\!-]", unlist(tmp.restrictions)))) stop("Error getting Restrictions: Non supported operators (+, -, *, /, !) found in restriction file.")
	if (any(vapply(no.white.restrictions, grepl, TRUE, pattern = "=.*<"), vapply(no.white.restrictions, grepl, TRUE, pattern = "<.*="), vapply(no.white.restrictions, grepl, TRUE, pattern = "=.*>"), vapply(no.white.restrictions, grepl, TRUE, pattern = ">.*="), vapply(no.white.restrictions, grepl, TRUE, pattern = ">.*<"), vapply(no.white.restrictions, grepl, TRUE, pattern = "<.*>"))) stop("Error getting restrictions: A line contains more than one of the following operators: =, <, >!")
	
	#recover()
	
	fixed.restrictions = list()
	equality.restrictions = list()
	inequality.restrictions = list()
	
	c.x.all <- 1
	c.restr <- 1
	#for (c.restr in 1:length(no.white.restrictions)) {
	regexp.number <- "^[[:digit:]]*\\.?[[:digit:]]+$"
	while (c.restr  < (length(no.white.restrictions)+1)) {
		tmp.restr <- strsplit(no.white.restrictions[[c.restr]], "[=><]")[[1]]
		if (any(grepl(regexp.number, tmp.restr[-length(tmp.restr)]))) stop(paste("Numerical constant not the rightmost element in restriction:", tmp.restrictions[[c.restr]]))
		if (grepl("=", no.white.restrictions[[c.restr]])) {
			if (grepl(regexp.number, tmp.restr[length(tmp.restr)])) {
				if (as.numeric(tmp.restr[length(tmp.restr)]) > 1 | as.numeric(tmp.restr[length(tmp.restr)]) < 0) stop("fixed restriction / numerical constant is not inside [0,1]")
				fixed.restrictions[[length(fixed.restrictions)+1]] <- new("fixed.restriction", parameter = tmp.restr[length(tmp.restr)-1], value = as.numeric(tmp.restr[length(tmp.restr)]))
				tmp.restr <- tmp.restr[-length(tmp.restr)]
			}
			if (length(tmp.restr) > 1) {
				for (c in 1:(length(tmp.restr)-1)) {
					equality.restrictions[[length(equality.restrictions)+1]] <- new("equality.restriction", parameter = tmp.restr[c], value = tmp.restr[c+1])
				}
			}
		}
		if (grepl("<", no.white.restrictions[[c.restr]])) {
			if (grepl(regexp.number, tmp.restr[length(tmp.restr)])) {
				no.white.restrictions[[length(no.white.restrictions)+1]] <- paste("hank", c.x.all, "=", tmp.restr[length(tmp.restr)], sep = "")
				tmp.restr[length(tmp.restr)] <- paste("hank", c.x.all, sep = "")
				c.x.all <- c.x.all + 1
			}
			for (c in 1:(length(tmp.restr)-1)) {
				#the replacement is done via "method a", Knapp & Batcheler (2004)
				inequality.restrictions[[length(inequality.restrictions)+1]] <- new("inequality.restriction", parameter = tmp.restr[c], exchange.inverse = list(paste(tmp.restr[c+1], "*(1-hank", c.x.all, ")", sep = ""), paste("(1-", tmp.restr[c+1], ")", sep ="")), exchange.parameter = list(paste(tmp.restr[c+1], "*hank", c.x.all, sep = "")), compute.as = parse(text = paste("hank", c.x.all, "*", tmp.restr[c+1], sep = "")))
				c.x.all <- c.x.all + 1
			}
		}
		if (grepl(">", no.white.restrictions[[c.restr]])) {
			if (grepl(regexp.number, tmp.restr[length(tmp.restr)])) {
				no.white.restrictions[[length(no.white.restrictions)+1]] <- paste("hank", c.x.all, "=", tmp.restr[length(tmp.restr)], sep = "")
				tmp.restr[length(tmp.restr)] <- paste("hank", c.x.all, sep = "")
				c.x.all <- c.x.all + 1
			}
			for (c in 1:(length(tmp.restr)-1)) {
				#the replacement is done via "method b", Knapp & Batcheler (2004)
				inequality.restrictions[[length(inequality.restrictions)+1]] <- new("inequality.restriction", parameter = tmp.restr[c], exchange.inverse = list(paste("(1-", tmp.restr[c+1],")*(1-hank",c.x.all, ")",  sep ="")), exchange.parameter = list(tmp.restr[c+1],paste("(1-", tmp.restr[c+1], ")*hank", c.x.all, sep = "")), compute.as = parse(text = paste("hank", c.x.all, "*", tmp.restr[c+1], sep = "")))
				c.x.all <- c.x.all + 1
			}
		}
		c.restr <- c.restr +1
	}
	new("restrictions", fixed = fixed.restrictions, equality = equality.restrictions, inequality = inequality.restrictions, raw = tmp.restrictions)
}



