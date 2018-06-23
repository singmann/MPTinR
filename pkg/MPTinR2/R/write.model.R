
setGeneric("write.mpt", function(model, ...) standardGeneric("write.mpt"))

setMethod("write.mpt", "mpt.model", function(model, eqn.filename, ...) {
	model.df <- model.data.frame(model)
	write.table(dim(model.df)[1], eqn.filename, row.names = FALSE, col.names = FALSE)
	write.table(model.df[,-4], eqn.filename, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
}
)
