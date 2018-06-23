require(devtools)
load_all()

data(rb.fig1.data, package = "MPTinR")
model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
# just fit the first dataset:
fit.mpt(rb.fig1.data[1,], model1, n.optim = 1, fia = 100)
fit.model(rb.fig1.data[1,], model1, n.optim = 1, fia = 100)


m1 <- fit.mpt(rb.fig1.data, model1, fia = 100000)
m1$information.criteria$sum
colSums(m1$information.criteria$individual)

select.mpt(list(m1, m1))
select.mpt(list(m1, m1), dataset = 2)

help(package = "MPTinR")
require("MPTinR")
data(d.broeder)
require(snowfall)
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
get.mpt.fia(d.broeder, m.2htm)


require(Rcpp)
compileAttributes() # DO NOT RUN THIS. It changes the names to determinant (without leading .) which needs to be changed by hand!

devtools::use_vignette("introduction")

devtools::build_vignettes()
