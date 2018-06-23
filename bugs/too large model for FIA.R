
require(MPTinR)

load("syll.meta.indiv.rda")
value.t1 <- 0.5

check.mpt("model1998.model", list(paste0("t1 = ", value.t1)))

make.mpt.cf("model1998.model")

(m1998.fia.indiv <- get.mpt.fia(dn[6, -1], "model1998.model", list(paste0("t1 = ", value.t1)), Sample=10, mConst=2^4))

(m1998.fia.indiv <- get.mpt.fia(dn[6, -1], "model1998.model", list(paste0("t1 = ", value.t1)), Sample=10))

str(m1998.fia.indiv)

(m1998..40.fia <- get.mpt.fia(dn[, 402:641], "model1998--40.model", list(paste0("t1 = ", value.t1)),  Sample=10000))

(m1998..40.fia <- get.mpt.fia(dn[, 402:641], "model1998--40.model", list(paste0("t1 = ", value.t1)),  Sample=10000, mConst=2^4))

(m1998.fia.indiv <- get.mpt.fia(dn[, -1], "model1998.model", list(paste0("t1 = ", value.t1)), Sample=10000, split = 10000))


prepare.mpt.fia(dn[6, -1], "model1998.model", list(paste0("t1 = ", value.t1)), Sample=10)