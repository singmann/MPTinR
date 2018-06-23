
# Example 1 (?fit.mpt): Proactive Inhibiton Model from Riefer & Batchelder (1988):
example1 <- function(max.diff, numerical.accuracy) {
	load("pkg/MPTinR/data/rb.fig1.data.rda")
	model1 <- "pkg/MPTinR/inst/extdata/rb.fig1.model"
	model1.eqn <- "pkg/MPTinR/inst/extdata/rb.fig1.model.eqn"
	rb.table1.res <- fit.mpt(rb.fig1.data, model1, n.optim = 1)
	load("testfiles/rb.table1.RData")
	e1.estimate <- round(t(rb.table1.res$parameters$individual[,1,]),2)
	(max(abs(rb.table1 - e1.estimate)) - max.diff) < numerical.accuracy
}

# Example 2 (?fit.mpt): Storage-Retrieval Model from Riefer & Batchelder (1988):
example2 <- function(numerical.accuracy) {
	load("pkg/MPTinR/data/rb.fig2.data.rda")
	model2 <- "pkg/MPTinR/inst/extdata/rb.fig2.model"
	model2r.r.eq <- "pkg/MPTinR/inst/extdata/rb.fig2.r.equal"
	model2r.c.eq <- "pkg/MPTinR/inst/extdata/rb.fig2.c.equal"
	ref.model <- fit.mpt(rb.fig2.data, model2, n.optim = 10)
	r.equal <- fit.mpt(rb.fig2.data, model2, model2r.r.eq, n.optim = 10)
	c.equal <- fit.mpt(rb.fig2.data, model2, model2r.c.eq, n.optim = 10)
	test.r <- round((r.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]]), 2) - 35.81 < numerical.accuracy
	test.c <- round((g.sq.c.equal <- c.equal[["goodness.of.fit"]][["G.Squared"]] - ref.model[["goodness.of.fit"]][["G.Squared"]]),2) - 1.43  < numerical.accuracy
	list(test.for.r = test.r, test.for.c = test.c)
}

# Example 3 (?fit.mpt): 2HTM Recognition Model with Data from Bröder & Schütz (2009, Experiment 3)
example.broeder.ex3 <- function(max.diff, numerical.accuracy) {
	load("pkg/MPTinR/data/d.broeder.rda")
	m.2htm <- "pkg/MPTinR/inst/extdata/5points.2htm.model"
	br.2htm <- fit.mpt(apply(d.broeder,2,sum), m.2htm, n.optim = 5)
	g2 <- (br.2htm$goodness.of.fit[["G.Squared"]] - 2.83 - max.diff) < numerical.accuracy
	ps <- (max(abs(br.2htm$parameters[,1] - c(.45, .56, .14, .22, .40, .60, .69))) - max.diff) < numerical.accuracy
	list(goodness.of.fit = g2, parameters = ps)
}

example.broeder.mptinr <- function(n.optim) {
	load("pkg/MPTinR/data/d.broeder.rda")
	m.2htm <- "pkg/MPTinR/inst/extdata/5points.2htm.model"
	r.2htm <- "pkg/MPTinR/inst/extdata/broeder.2htm.restr"
	r.1htm <- "pkg/MPTinR/inst/extdata/broeder.1htm.restr"
	i.2htm <- "pkg/MPTinR/inst/extdata/broeder.2htm.ineq"
	ir.2htm <- "pkg/MPTinR/inst/extdata/broeder.2htm.restr.ineq"
	ir.1htm <- "pkg/MPTinR/inst/extdata/broeder.1htm.restr.ineq"
	assign("br.2htm.test", fit.mpt(d.broeder, m.2htm, n.optim = n.optim), envir = .GlobalEnv)
	assign("br.2htm.ineq.test", fit.mpt(d.broeder, m.2htm, i.2htm, n.optim = n.optim), envir = .GlobalEnv)
	assign("br.2htm.res.test", fit.mpt(d.broeder, m.2htm, r.2htm, n.optim = n.optim), envir = .GlobalEnv)
	assign("br.2htm.res.ineq.test", fit.mpt(d.broeder, m.2htm, ir.2htm, n.optim = n.optim), envir = .GlobalEnv)
	assign("br.1htm.test", fit.mpt(d.broeder, m.2htm, r.1htm, n.optim = n.optim), envir = .GlobalEnv)
	assign("br.1htm.ineq.test", fit.mpt(d.broeder, m.2htm, ir.1htm, n.optim = n.optim), envir = .GlobalEnv)
}

