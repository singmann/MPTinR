
b.model <- "
p * q * r
p * q * (1-r)
p * (1-q) * r
p * (1-q) * (1-r) + (1-p)
"


# load the data
data(rb.fig1.data, package = "MPTinR")


fit.mpt(rb.fig1.data, textConnection(b.model), fia = 200000)
