# The first example fits the MPT model presented in Riefer and Batchelder (1988, Figure 1)
# to the data presented in Riefer and Batchelder (1988, Table 1)
# Note that Riefer and Batchelder (1988, pp. 328) did some hypotheses tests, that are not done here.
# Rather, we use each condition (i.e., row in Table 1) as a different individual.


# load the data
data(rb.fig1.data, package = "MPTinR2")

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
# see ?make.mpt for more on how to specify a model and restrictions

# just fit the first "individual":
fit.mpt(model1, rb.fig1.data[1,])

#fit all "individuals":
fit.mpt(model1, rb.fig1.data)

#fit all "individuals" using the .EQN model file:
fit.mpt(model1.eqn, rb.fig1.data)

#fit all "individuals" using the .txtCon model file:
fit.mpt(model1.txtCon, rb.fig1.data)


# The second example fits the MPT model presented in Riefer and Batchelder (1988, Figure 2)
# to the data presented in Riefer and Batchelder (1988, Table 3)
# First, the model without restrictions is fitted: ref.model
# Next, the model with all r set equal is fitted: r.equal
# Then, the model with all c set equal is fitted: c.equal
# Finally, the inferential tests reported by Riefer & Batchelder, (1988, p. 332) are executed.


# get the data
data(rb.fig2.data, package = "MPTinR2")

# make model objects
model2.file <- system.file("extdata", "rb.fig2.model", package = "MPTinR2")
model2 <- make.mpt(model2.file)
model2.r.eq <- make.mpt(model2.file, system.file("extdata", "rb.fig2.r.equal", package = "MPTinR2"))
model2.c.eq <- make.mpt(model2.file, system.file("extdata", "rb.fig2.c.equal", package = "MPTinR2"))

# The full (i.e., unconstrained) model
(ref.model <- fit.mpt(model2, rb.fig2.data))


# All r equal
(r.equal <- fit.mpt(model2.r.eq, rb.fig2.data))

# All c equal
(c.equal <- fit.mpt(model2.c.eq, rb.fig2.data))

# is setting all r equal a good idea?
(g.sq.r.equal <- goodness.of.fit(r.equal)[["G.Squared"]] - goodness.of.fit(ref.model)[["G.Squared"]])
(df.r.equal <- goodness.of.fit(r.equal)[["df.model"]] - goodness.of.fit(ref.model)[["df.model"]])
(p.value.r.equal <- pchisq(g.sq.r.equal, df.r.equal , lower.tail = FALSE))

# is setting all c equal a good idea?
(g.sq.c.equal <- goodness.of.fit(c.equal)[["G.Squared"]] - goodness.of.fit(ref.model)[["G.Squared"]])
(df.c.equal <- goodness.of.fit(c.equal)[["df.model"]] - goodness.of.fit(ref.model)[["df.model"]])
(p.value.c.equal <- pchisq(g.sq.c.equal, df.c.equal , lower.tail = FALSE))

# Example from Broeder & Schuetz (2009)
# We fit the data from the 40 individuals from their Experiment 3
# We fit three different models:
# 1. Their 2HTM model: br.2htm
# 2. A restricted 2HTM model with Dn = Do: br.2htm.res
# 3. A 1HTM model (i.e., Dn = 0): br.1htm
# We fit the models with, as well as without, applied inequality restrictions (see Details)
# that is, for some models (.ineq) we impose: G1 < G2 < G3 < G4 < G5 
# As will be apparent, the inequality restrictions do not hold for all individuals.

data(d.broeder, package = "MPTinR2")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR2")
r.2htm <- system.file("extdata", "broeder.2htm.restr", package = "MPTinR2")
r.1htm <- system.file("extdata", "broeder.1htm.restr", package = "MPTinR2")
i.2htm <- system.file("extdata", "broeder.2htm.ineq", package = "MPTinR2")
ir.2htm <- system.file("extdata", "broeder.2htm.restr.ineq", package = "MPTinR2")
ir.1htm <- system.file("extdata", "broeder.1htm.restr.ineq", package = "MPTinR2")

# fit the original 2HTM
br.2htm <- fit.mpt(m.2htm, d.broeder)
br.2htm.ineq <- fit.mpt(m.2htm, d.broeder, i.2htm)

# do the inequalities hold for all participants?
parameters(br.2htm.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",]
parameters(br.2htm)[["individual"]][,"estimates",]
# See the difference between forced and non-forced inequality restrictions:
round(parameters(br.2htm)[["individual"]][,"estimates",]-
parameters(br.2htm.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",],2)

# The same for the other two models
# The restricted 2HTM
br.2htm.res <- fit.mpt(m.2htm, d.broeder, r.2htm)
br.2htm.res.ineq <- fit.mpt(m.2htm, d.broeder, ir.2htm)
round(parameters(br.2htm.res, sort.alphabetical = TRUE)[["individual"]][,"estimates",]-
parameters(br.2htm.res.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",],2)

# The 1HTM
br.1htm <- fit.mpt(m.2htm, d.broeder, r.1htm)
br.1htm.ineq <- fit.mpt(m.2htm, d.broeder, ir.1htm)
round(parameters(br.1htm, sort.alphabetical = TRUE)[["individual"]][,"estimates",]-
parameters(br.1htm.ineq, sort.alphabetical = TRUE)[["individual"]][,"estimates",],2)
