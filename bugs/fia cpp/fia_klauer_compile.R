
require("MPTinR")

catdat<-c( 180,  71,  70,  65,  66,  57,  27,  37,  48, 21, 23, 23) # data for the categories in the order of the rows in the equation file

check.mpt("2htm_k=6.model")

get.mpt.fia(catdat,model.filename="2htm_k=6.model")

fit.mpt(catdat,model.filename="2htm_k=6.model")


require("MPTinR")
data(d.broeder)
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
get.mpt.fia(d.broeder, m.2htm, restrictions.filename = list(NULL))
get.mpt.fia(d.broeder, m.2htm, mConst = 2L^8)

get.mpt.fia(d.broeder, m.2htm, i.2htm)

# should produce very similar results:
get.mpt.fia(d.broeder, m.2htm, i.2htm, mConst = 2L^8)

