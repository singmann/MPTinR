

#############
## General ##
#############

.find.MPT.params <- function(model) {
	inobjects <- function(ex) {
		split.exp1 <- strsplit(as.character(ex),"[[:space:]]")
		split.exp2 <- sapply(split.exp1, strsplit, split = "[()+*-]")
		return(sort(unique(grep("[[:alpha:]]",unlist(split.exp2), value = TRUE))))
		}
	tmp <- sapply(model,inobjects)
	return(unique(sort(unique(unlist(tmp)))))
}


.count.branches <- function(model.df) {
	counters <- rep(1, max(model.df[,"category"]))
	for (branch in 1:dim(model.df)[1]) {
		tmp.cat <- model.df[branch,"category"]
		model.df[branch,"n.branch"] <- counters[tmp.cat]
		counters[tmp.cat] <- counters[tmp.cat] + 1
	}
	model.df
}
