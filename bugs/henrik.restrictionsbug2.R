
rebug2.data <- structure(c(8, 4, 3, 8, 6, 2, 6, 7, 2, 4, 3, 1, 1, 9, 5, 17, 
19, 19, 11, 15, 14, 11, 12, 11, 12, 0, 3, 2, 3, 2, 4, 6, 4, 10, 
7, 10, 8, 10, 4, 7, 18, 19, 11, 12, 17, 2, 1, 9, 8, 3, 4, 4, 
2, 7, 4, 6, 6, 8, 3, 6, 7, 6, 8, 7, 6, 3, 4, 2, 3, 4, 3, 4, 3, 
8, 2, 17, 16, 17, 12, 18, 10, 11, 9, 13, 9, 4, 3, 5, 1, 5, 1, 
4, 3, 4, 3, 13, 10, 11, 10, 11, 16, 18, 16, 14, 13, 4, 2, 4, 
6, 7, 1, 0, 3, 2, 3, 9, 10, 7, 8, 7), .Dim = c(5L, 24L), .Dimnames = list(
    c("ore1/data/ore1/ore1_3.rtd", "ore1/data/ore1/ore1_4.rtd", 
    "ore1/data/ore1/ore1_5.rtd", "ore1/data/ore1/ore1_6.rtd", 
    "ore1/data/ore1/ore1_7.rtd"), NULL))

rebug2.model <- "\nDo.a + (1-Do.a)*G1.a\n(1-Do.a)*(1-G1.a)\n\n(1-Dn.a)*G1.a\nDn.a + (1-Dn.a)*(1-G1.a)\n\nDo.a + (1-Do.a)*G2.a\n(1-Do.a)*(1-G2.a)\n\n(1-Dn.a)*G2.a\nDn.a + (1-Dn.a)*(1-G2.a)\n\nDo.a + (1-Do.a)*G3.a\n(1-Do.a)*(1-G3.a)\n\n(1-Dn.a)*G3.a\nDn.a + (1-Dn.a)*(1-G3.a)\n\nDo.g + (1-Do.g)*G1.g\n(1-Do.g)*(1-G1.g)\n\n(1-Dn.g)*G1.g\nDn.g + (1-Dn.g)*(1-G1.g)\n\nDo.g + (1-Do.g)*G2.g\n(1-Do.g)*(1-G2.g)\n\n(1-Dn.g)*G2.g\nDn.g + (1-Dn.g)*(1-G2.g)\n\nDo.g + (1-Do.g)*G3.g\n(1-Do.g)*(1-G3.g)\n\n(1-Dn.g)*G3.g\nDn.g + (1-Dn.g)*(1-G3.g)\n\n"


# evoked the bug (restricted paramter not present at position 2)
fit.mpt(rebug2.data, textConnection(rebug2.model),  list("G1.a = G1.d", "G2.a = G2.d", "G3.a = G3.d"))

# now works
fit.mpt(rebug2.data, textConnection(rebug2.model),  list("G2.d = G2.a = G3.a = G3.d"))
