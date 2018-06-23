
require(RcppEigen)
require(inline)
require(snowfall)

for (r.file in list.files("pkg/MPTinR/R", full.names = TRUE)) {
	source(r.file)
}

dyn.load("pkg/MPTinR/src-x64/MPTinR.dll")
dyn.unload("pkg/MPTinR/src-x64/MPTinR.dll")

options(error = recover)
options(error = NULL)

# Source testfunctions
source("test_functions_fitting.R")
numerical.accuracy <- 1e-8

################################################################
## Test 1: Test fitting against references in the literature: ##
################################################################

# Example 1 (?fit.mpt): Proactive Inhibiton Model from Riefer & Batchelder (1988):
# The parameter values for proactive inhibition model are tested against reference (Riefer & Batchelder,1988, Table 1)
# Maximal accepted difference in parameter values is <= .01
suppressWarnings(example1(.01, numerical.accuracy))

# Example 2 (?fit.mpt): Storage-Retrieval Model from Riefer & Batchelder (1988):
# This tests compares the likelihood ratios tests for two restricted models (setting either r or c equal)
# with the reference in Batchelder & Riefer (1988, p. 332)
suppressWarnings(example2(numerical.accuracy))

# Example 3 (?fit.mpt): 2HTM Recognition Model with Data from Bröder & Schütz (2009, Experiment 3)
# This tests compares the G² values and the parameter values from Broeder & Schütz (2009, Experiment 3)
# with the reference in their paper (Table 4)
example.broeder.ex3(0.01, numerical.accuracy)


#######################################################################################
## Test 2: Test fitting of multi-individual fit against reference result from MPTinR ##
#######################################################################################

# current reference was calculated with MPTinR 0.6.3
# following tests can give small differences 

load("testfiles/bs_ref_063.RData")

example.broeder.mptinr(20)

all.equal(br.2htm.ref[-6], br.2htm.test[-6], tolerance = 0.01)
all.equal(br.2htm.ineq.ref[-6], br.2htm.ineq.test[-6], tolerance = 0.01)

all.equal(br.2htm.res.ref[-6], br.2htm.res.test[-6], tolerance = 0.001)
all.equal(br.2htm.res.ineq.ref[-6], br.2htm.res.ineq.test[-6], tolerance = 0.001)

all.equal(br.1htm.ref[-6], br.1htm.test[-6], tolerance = 0.001)
all.equal(br.1htm.ineq.ref[-6], br.1htm.ineq.test[-6], tolerance = 0.001)



############################################################
## Test 3: Testing the FIA function (lengthy operations!) ##
############################################################

source("test_functions_FIA.R")

# FIA Example 1: Reference
# Compare the FIA for a 1HTM with the reference results in Wu, Myung, & Batchelder (2010, pp. 280)
# reference value is 12.6182, CI from 12.6113 - 12.6251 (p. 281)
# running this again in Matlab gives: 12.6252, CI: 12.6190 - 12.6313
# a reference run in MPTinR's bmpt.fia (v 0.6.3) gives: 12.6230, CI 12.6169 - 12.6292
# Therefore, I assume that the value should always fall within 12.61 and 12.63
test.wmb.ex1(12.61, 12.63)

# FIA Example 2: A single big tree
# Test if bmpt.fia works for a single, rather big tree (the dual-process model from Oberauer, 2006).
# Matlab returns a FIA of 19.4398 with a CI from 19.4260 to 19.4537
# MPTinR reference is: 19.44 with no CI (seems to be a bug!)
# Result should be between 19.43 and 19.445 
test.oberauer2006(19.43, 19.445)

# FIA Example 3: Single parameter models
# This tests test a small model (3 trees) of the take-the-best heuristic with either
# 1 free and 1 fixed parameters (Matlab reference value: 2.38 +/- .01)
# 1 free and 0 fixed parameters (Matlab reference value: 3.07 +/- .01)
# 2 free and 0 fixed parameters (Matlab reference value: -13.895 +/- .01)
# The first of the three trees with one single parameter (Matlab reference value: 2.527 +/- .01)
# These tests are necessary because of Rs behavior of '[' with drop = TRUE (which is the default)
test.ttb(2.38, 3.07, -13.895, 2.527, 0.01)



