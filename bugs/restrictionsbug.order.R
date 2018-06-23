
# The bug is invoked when setting an inequality restriction to a parameter
# that is also set equal.

data(d.broeder, package = "MPTinR")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")

check.mpt(m.2htm)

fit.mpt(d.broeder[1:3,], m.2htm, list("G2 < G3 < G5", "G1 = G2", "G4 = G5"))

check.mpt(m.2htm, list("G2 < G3 < G5", "G1 = G2", "G4 = G5"))
