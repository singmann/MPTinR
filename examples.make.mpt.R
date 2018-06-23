
# These examples make model objects from two models from Riefer and Batchelder (1988)
# for fitting and more information regarding the models see ?fit.mpt

#make model objects, once using the easy format, once using the eqn format.
(model1 <- make.mpt(system.file("extdata", "rb.fig1.model", package = "MPTinR2")))
(model1.eqn <- make.mpt(system.file("extdata", "rb.fig1.model.eqn", package = "MPTinR2")))
#both models are identical:
identical(model1, model1.eqn)

# specify the same model via textConnection
model1.txtCon <- make.mpt(textConnection("p * q * r
p * q * (1-r)
p * (1-q) * r
p * (1-q) * (1-r) + (1-p)"))
identical(model1, model1.txtCon)


#get the location of model file 2 and restrictions for that model:
model2.file <- system.file("extdata", "rb.fig2.model", package = "MPTinR2")
model2.restrictions.file <- system.file("extdata", "rb.fig2.r.equal", package = "MPTinR2")

# make model objects via files:
model2 <- make.mpt(model2.file)
model2.r.eq <- make.mpt(model2.file, model2.restrictions.file)

# make model object with text connection and restrictions with text connections:
model2.r.eq.txtCon <- make.mpt(textConnection("
			#trees for condition 0:
			c0 * r0
			(1 - c0) * u0 * u0
			(1 - c0) * u0 * (1 - u0) + (1 - c0) * (1 - u0) * u0
			c0 * (1-r0) + (1-c0) * (1-u0) * (1 - u0)

			u0
			(1 - u0)

			#trees for condition 1:
			c1 * r1
			(1 - c1) * u1 * u1
			(1 - c1) * u1 * (1 - u1) + (1 - c1) * (1 - u1) * u1
			c1 * (1-r1) + (1-c1) * (1-u1) * (1 - u1)

			u1
			(1 - u1)

			#trees for condition 2:
			c2 * r2
			(1 - c2) * u2 * u2
			(1 - c2) * u2 * (1 - u2) + (1 - c2) * (1 - u2) * u2
			c2 * (1-r2) + (1-c2) * (1-u2) * (1 - u2)

			u2
			(1 - u2)

			#trees for condition 3:
			c3 * r3
			(1 - c3) * u3 * u3
			(1 - c3) * u3 * (1 - u3) + (1 - c3) * (1 - u3) * u3
			c3 * (1-r3) + (1-c3) * (1-u3) * (1 - u3)

			u3
			(1 - u3)

			#trees for condition 4:
			c4 * r4
			(1 - c4) * u4 * u4
			(1 - c4) * u4 * (1 - u4) + (1 - c4) * (1 - u4) * u4
			c4 * (1-r4) + (1-c4) * (1-u4) * (1 - u4)

			u4
			(1 - u4)"), textConnection("r0 = r1 = r2 = r3 = r4"))

# make model object with restrictions as list
model2.r.eq.list <- make.mpt(model2.file, list("r0 = r1 = r2 = r3 = r4"))

identical(model2.r.eq, model2.r.eq.txtCon)
identical(model2.r.eq, model2.r.eq.list)
