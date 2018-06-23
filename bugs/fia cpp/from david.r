DMAT <- structure(c(69, 79, 78, 61, 135, 141, 157, 156, 71, 72, 81, 80, 
45, 36, 36, 53, 105, 99, 83, 84, 67, 66, 57, 58, 45, 30, 52, 
72, 99, 103, 154, 173, 45, 51, 64, 86, 69, 84, 62, 42, 141, 137, 
86, 67, 93, 87, 74, 52, 73, 55, 55, 61, 100, 143, 133, 156, 52, 
52, 58, 73, 41, 59, 59, 53, 140, 97, 107, 84, 86, 86, 80, 65, 
94, 99, 99, 86, 139, 164, 157, 179, 77, 74, 87, 91, 20, 16, 15, 
28, 101, 76, 83, 64, 61, 64, 51, 47, 25, 44, 44, 42, 86, 97, 
88, 122, 44, 44, 44, 61, 89, 70, 70, 72, 154, 143, 152, 118, 
94, 94, 94, 77, 62, 62, 70, 65, 101, 109, 120, 143, 49, 74, 64, 
73, 52, 52, 44, 49, 139, 131, 120, 97, 89, 64, 74, 65, 48, 46, 
44, 47, 99, 103, 121, 105, 49, 52, 47, 53, 66, 68, 70, 67, 141, 
137, 118, 135, 89, 86, 91, 85, 82, 92, 91, 87, 150, 179, 122, 
161, 75, 87, 68, 85, 32, 22, 23, 27, 90, 61, 118, 79, 63, 51, 
70, 53, 35, 41, 31, 62, 74, 103, 93, 103, 41, 56, 69, 60, 79, 
73, 83, 52, 166, 137, 147, 137, 97, 82, 138, 78, 51, 69, 50, 
69, 120, 145, 131, 156, 59, 59, 47, 81, 63, 45, 64, 45, 120, 
95, 109, 84, 79, 79, 91, 57, 64, 57, 70, 71, 86, 118, 132, 135, 
59, 66, 68, 107, 50, 57, 44, 43, 154, 122, 110, 105, 79, 72, 
70, 100, 90, 82, 90, 90, 154, 131, 152, 159, 84, 74, 79, 82, 
24, 32, 24, 24, 86, 109, 88, 81, 54, 64, 59, 56, 59, 117, 46, 
68, 55, 101, 47, 70, 59, 104, 48, 76, 283, 226, 296, 274, 287, 
241, 295, 272, 355, 310, 366, 338, 76, 97, 43, 61, 55, 91, 38, 
81, 71, 99, 45, 85, 266, 239, 299, 281, 287, 251, 304, 261, 343, 
315, 369, 329, 59, 109, 29, 62, 68, 95, 45, 76, 87, 100, 52, 
69, 283, 227, 313, 280, 274, 257, 297, 266, 327, 314, 362, 345, 
72, 141, 52, 82, 79, 116, 47, 77, 89, 109, 61, 80, 270, 201, 
290, 260, 263, 226, 295, 265, 325, 305, 353, 334), .Dim = c(12L, 
32L), .Dimnames = list(NULL, c("L1_pL1_data", "L1_pL1_complement", 
"L1_pL2_data", "L1_pL2_complement", "L1_pL3_data", "L1_pL3_complement", 
"L1_pL1UL2UL3_data", "L1_pL1UL2UL3_complement", "L2_pL1_data", 
"L2_pL1_complement", "L2_pL2_data", "L2_pL2_complement", "L2_pL3_data", 
"L2_pL3_complement", "L2_pL1UL2UL3_data", "L2_pL1UL2UL3_complement", 
"L3_pL1_data", "L3_pL1_complement", "L3_pL2_data", "L3_pL2_complement", 
"L3_pL3_data", "L3_pL3_complement", "L3_pL1UL2UL3_data", "L3_pL1UL2UL3_complement", 
"Unrelated_pL1_data", "Unrelated_pL1_complement", "Unrelated_pL2_data", 
"Unrelated_pL2_complement", "Unrelated_pL3_data", "Unrelated_pL3_complement", 
"Unrelated_pL1UL2UL3_data", "Unrelated_pL1UL2UL3_complement")))

H1TM3L <- "

	############
	# Source A #
	############

	# probe A #

	Da*da + Da*(1-da)*ja + (1-Da)*ga
	Da*(1-da)*(1-ja) + (1-Da)*(1-ga)

	# probe B #

	Da*(1-da)*jb + (1-Da)*gb
	Da*da + Da*(1-da)*(1-jb) + (1-Da)*(1-gb)


	# probe C #

	Da*(1-da)*jc + (1-Da)*gc
	Da*da + Da*(1-da)*(1-jc) + (1-Da)*(1-gc)	
		
	# probe Old #

	Da + (1-Da)*gi
	(1-Da)*(1-gi)

	############
	# Source B #
	############

	# probe A #

	Db*(1-db)*ja + (1-Db)*ga
	 Db*db + Db*(1-db)*(1-ja) + (1-Db)*(1-ga)

	# probe B #
	   
	Db*db + Db*(1-db)*jb + (1-Db)*gb
	Db*(1-db)*(1-jb) + (1-Db)*(1-gb)

		
	# probe C #

	Db*(1-db)*jc + (1-Db)*gc
	Db*db + Db*(1-db)*(1-jc) + (1-Db)*(1-gc)


	# probe Old #

	Db + (1-Db)*gi
	(1-Db)*(1-gi)
		
	############
	# Source c #
	############

	# probe A #

	Dc*(1-dc)*ja + (1-Dc)*ga
	Dc*dc + Dc*(1-dc)*(1-ja) + (1-Dc)*(1-ga)

	# probe B #
	   
	Dc*(1-dc)*jb + (1-Dc)*gb
	Dc*dc + Dc*(1-dc)*(1-jb) + (1-Dc)*(1-gb)

		
	# probe C #

	Dc*dc + Dc*(1-dc)*jc + (1-Dc)*gc
	Dc*(1-dc)*(1-jc) + (1-Dc)*(1-gc)

		
	# probe old #
	   
	Dc + (1-Dc)*gi
	(1-Dc)*(1-gi)

		
		
	#######
	# New #
	#######
	
	# probe A #

	(1-dn)*ga
	dn + (1-dn)*(1-ga)

	# probe B #
	   
	(1-dn)*gb
	dn + (1-dn)*(1-gb)

	# probe C #
	   
	(1-dn)*gc
	dn + (1-dn)*(1-gc)
		
	# probe old #
	   
	(1-dn)*gi
	dn + (1-dn)*(1-gi)
"

require(devtools)
dev_mode()
require(MPTinR)

get.mpt.fia(DMAT, textConnection(CPDM3L), Sample= 100)

CPDM3L <- "



	############
	# Source A #
	############

	# probe A #

	 Ra + (1-Ra)*(1-Eb)*(1-Ec)*Ia + (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*ga
	 (1-Ra)*Eb + (1-Ra)*(1-Eb)*Ec + (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*(1-ga)

	# probe B #

	(1-Ra)*Eb + (1-Ra)*(1-Eb)*(1-Ec)*Ia + (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*gb
	Ra + (1-Ra)*(1-Eb)*Ec + (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*(1-gb)

	# probe C #

	(1-Ra)*(1-Eb)*Ec + (1-Ra)*(1-Eb)*(1-Ec)*Ia +  (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*gc
	Ra + (1-Ra)*Eb + (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*(1-gc)
		
	# probe Old #

	Ra + (1-Ra)*Eb + (1-Ra)*(1-Eb)*Ec + (1-Ra)*(1-Eb)*(1-Ec)*Ia + (1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*gi
	(1-Ra)*(1-Eb)*(1-Ec)*(1-Ia)*(1-gi)

	############
	# Source B #
	############

	# probe A #

	(1-Rb)*Ea + (1-Rb)*(1-Ea)*(1-Ec)*Ib + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*ga
	 Rb + (1-Rb)*(1-Ea)*Ec + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*(1-ga)

	# probe B #
	   
	Rb + (1-Rb)*(1-Ea)*(1-Ec)*Ib + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*gb
	(1-Rb)*Ea + (1-Rb)*(1-Ea)*Ec + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*(1-gb)

		
	# probe C #

	(1-Rb)*(1-Ea)*Ec + (1-Rb)*(1-Ea)*(1-Ec)*Ib + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*gc
	Rb + (1-Rb)*Ea + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*(1-gc)


	# probe Old #

	Rb + (1-Rb)*Ea + (1-Rb)*(1-Ea)*Ec + (1-Rb)*(1-Ea)*(1-Ec)*Ib + (1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*gi
	(1-Rb)*(1-Ea)*(1-Ec)*(1-Ib)*(1-gi)
		
	############
	# Source c #
	############

	# probe A #

	(1-Rc)*Ea + (1-Rc)*(1-Ea)*(1-Eb)*Ic + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*ga
	Rc + (1-Rc)*(1-Ea)*Eb + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*(1-ga)

	# probe B #
	   
	(1-Rc)*(1-Ea)*Eb + (1-Rc)*(1-Ea)*(1-Eb)*Ic + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*gb
	Rc + (1-Rc)*Ea + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*(1-gb)

		
	# probe C #

	Rc + (1-Rc)*(1-Ea)*(1-Eb)*Ic + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*gc
	(1-Rc)*Ea + (1-Rc)*(1-Ea)*Eb + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*(1-gc)

		
	# probe old #
	   
	Rc + (1-Rc)*Ea + (1-Rc)*(1-Ea)*Eb +(1-Rc)*(1-Ea)*(1-Eb)*Ic + (1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*gi
	(1-Rc)*(1-Ea)*(1-Eb)*(1-Ic)*(1-gi)

		
		
	#######
	# New #
	#######

	# probe A #

	(1-dn)*ga
	dn + (1-dn)*(1-ga)

	# probe B #
	   
	(1-dn)*gb
	dn + (1-dn)*(1-gb)

	# probe C #
	   
	(1-dn)*gc
	dn + (1-dn)*(1-gc)
		
	# probe old #
	   
	(1-dn)*gi
	dn + (1-dn)*(1-gi)

"

x <- fit.mpt(DMAT,textConnection(H1TM3L), n.optim=20, rest=list("dn=0"), fit.aggregated = FALSE, fia=200000)
y <- fit.mpt(DMAT,textConnection(CPDM3L), n.optim=20, rest=list("dn=0"), fit.aggregated = FALSE, fia=200000)

select.mpt(list(htm.r = x, cpdm.r = y))

xf <- fit.mpt(DMAT,textConnection(H1TM3L), n.optim=20, fit.aggregated = FALSE, fia=200000)
yf <- fit.mpt(DMAT,textConnection(CPDM3L), n.optim=20, fit.aggregated = FALSE, fia=200000)

select.mpt(list(htm = xf, htm.r = x, cpdm = yf, cpdm.r = y), output = "full")

##################################################################################################################
# IGNORE THIS. THIS IS HOW I COMPUTED THE LAPLACE APPROXIMATION. ALSO CONSIDER THE FULL MODELS (NO RESTRICTIONS) #
##################################################################################################################
library(MPTinR)

x <- fit.mpt(DMAT,textConnection(H1TM3L), n.optim=20, rest=list("dn=0"), output="full",fit.aggregated = FALSE)
y <- fit.mpt(DMAT,textConnection(CPDM3L), n.optim=20, rest=list("dn=0"), output="full",fit.aggregated = FALSE)


ddx <- c()
ddy <- c()
for(i in 1:12) ddx[i] <- sqrt(det(x$hessian[[i]]))
for(i in 1:12) ddy[i] <- sqrt(det(y$hessian[[i]]))