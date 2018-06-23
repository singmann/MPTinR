
test.wmb.ex1 <- function(lower, upper) {
	model.1htm <- "pkg/MPTinR/inst/extdata/wmb.ex1.model"
	model.1htm.restr <- "pkg/MPTinR/inst/extdata/wmb.ex1.restr"
	fia.ex1 <- get.mpt.fia(c(250,0,0,250,0,0,500,0,0), model.1htm, model.1htm.restr)
	fia.ex1[1,1] > lower & fia.ex1[1,1] < upper
}

test.oberauer2006 <- function(lower, upper) {
	oberauer.dual <- get.mpt.fia(c(557, rep(0,15)), "testfiles/oberauer2006.dual.model")
	oberauer.dual[1,1] > lower & oberauer.dual[1,1] < upper
}

test.ttb <- function(ref1, ref2, ref3, ref4, tolerance) {
	ttb1 <- tryCatch(get.mpt.fia(rep(c(100,0),3), "testfiles/ttb.model", "testfiles/ttb.1.restr"), error = function(e) {print(e); matrix(NA)})
	ttb1.res <- (ttb1[1,1] > ref1 - tolerance & ttb1[1,1] < ref1 + tolerance) 
	ttb2 <- tryCatch(get.mpt.fia(rep(c(100,0),3), "testfiles/ttb.model", "testfiles/ttb.2.restr"), error = function(e) {print(e); matrix(NA)})
	ttb2.res <- (ttb2[1,1] > ref2 - tolerance & ttb2[1,1] < ref2 + tolerance) 
	ttb3 <- tryCatch(get.mpt.fia(rep(c(100,0),3), "testfiles/ttb.model"), error = function(e) {print(e); matrix(NA)})
	ttb3.res <- (ttb3[1,1] > ref3 - tolerance & ttb3[1,1] < ref3 + tolerance) 
	ttb.minimal <- tryCatch(get.mpt.fia(c(100,0), "testfiles/ttb.minimal.model", "testfiles/ttb.2.restr"), error = function(e) {print(e); matrix(NA)})
	ttb4.res <- (ttb.minimal[1,1] > ref4 - tolerance & ttb.minimal[1,1] < ref4 + tolerance) 
	list(ttb1 = ttb1.res, ttb2 = ttb2.res, ttb3 = ttb3.res, ttb4 = ttb4.res)
}

