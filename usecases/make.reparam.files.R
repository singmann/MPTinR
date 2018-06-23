

data(d.broeder, package = "MPTinR2")
m.2htm.f <- system.file("extdata", "5points.2htm.model", package = "MPTinR2")

# fit the original 2HTM
m.2htm <- make.mpt(m.2htm.f, list("G1 < G2", "Do = Dn"))

fit.mpt(m.2htm, colSums(d.broeder))

write.mpt(m.2htm, "testfile.eqn")

make.mpt("testfile.eqn")




