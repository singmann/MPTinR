
model1.bmpt <- "pkg/inst/extdata/rb.fig2.model"

model1.mpt <- "pkg/inst/extdata/rb.fig2.model2"


m1.bmpt <- .read.MPT.model(model1.bmpt)

bmpt1 <- .make.mpt.cf(m1.bmpt)

.prepare.mpt.fia(bmpt1)


prepare.mpt.fia(c(250,0,0,250,0,0,500,0,0), model.1htm, model.1htm.restr)
