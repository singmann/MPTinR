
data(d.broeder, package = "MPTinR")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")

check.mpt(m.2htm)

d.broeder.agg <- colSums(d.broeder)

# fit the original 2HTM
br.2htm <- fit.mpt(d.broeder.agg, m.2htm, fia = 20000)

