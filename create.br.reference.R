
load("pkg/data/d.broeder.rda")
m.2htm <- "pkg/inst/extdata/5points.2htm.model"
r.2htm <- "pkg/inst/extdata/broeder.2htm.restr"
r.1htm <- "pkg/inst/extdata/broeder.1htm.restr"
i.2htm <- "pkg/inst/extdata/broeder.2htm.ineq"
ir.2htm <- "pkg/inst/extdata/broeder.2htm.restr.ineq"
ir.1htm <- "pkg/inst/extdata/broeder.1htm.restr.ineq"

n.optim <- 20

br.2htm.ref <- fit.mpt(d.broeder, m.2htm, n.optim = n.optim)
br.2htm.ineq.ref <- fit.mpt(d.broeder, m.2htm, i.2htm, n.optim = n.optim)
br.2htm.res.ref <- fit.mpt(d.broeder, m.2htm, r.2htm, n.optim = n.optim)
br.2htm.res.ineq.ref <- fit.mpt(d.broeder, m.2htm, ir.2htm, n.optim = n.optim)
br.1htm.ref <- fit.mpt(d.broeder, m.2htm, r.1htm, n.optim = n.optim)
br.1htm.ineq.ref <- fit.mpt(d.broeder, m.2htm, ir.1htm, n.optim = n.optim)


save(list=ls(pattern="ref"),file="testfiles/bs_ref_063.RData")


