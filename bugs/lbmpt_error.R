
require(MPTinR)

mmt <- "# 5:AI1
(1 - r.5)* g1 * g2 * g3 * g4                   # Aac
(1 - r.5)* g1 * g2 * g3 * (1-g4)               # Eac
(1 - r.5)* g1* g2 *(1-g3)                      + r.5 * t1 * x.5.1 + r.5 * (1-t1) * x.5.2 * x.5.1 # Iac
(1 - r.5)* g1* (1-g2) * g5                     # Oac
(1 - r.5)* g1 * (1-g2)* (1- g5)                # Aca
(1 - r.5)* (1-g1) * g6 * g7                    # Eca
(1 - r.5)* (1-g1) * g6 * (1-g7)                + r.5 * t1 * (1-x.5.1) +  r.5 * (1-t1) * x.5.2 * (1-x.5.1) # Ica
(1 - r.5)* (1-g1) * (1-g6) * g8                # Oca
(1 - r.5)* (1-g1) * (1-g6) * (1-g8) * g9       + r.5 * (1-t1) *  (1-x.5.2) # NVC
(1 - r.5)* (1-g1) * (1-g6) * (1-g8) * (1-g9)   # MISC


# 21:II1
(1 - r.21)* g1 * g2 * g3 * g4                   # Aac
(1 - r.21)* g1 * g2 * g3 * (1-g4)               # Eac
(1 - r.21)* g1* g2 *(1-g3)                      + r.21 * t1 * x.21.1 +   r.21 * (1-t1) * x.21.2 * x.21.1 # Iac
(1 - r.21)* g1* (1-g2) * g5                     # Oac
(1 - r.21)* g1 * (1-g2)* (1- g5)                # Aca
(1 - r.21)* (1-g1) * g6 * g7                    # Eca
(1 - r.21)* (1-g1) * g6 * (1-g7)                + r.21 * t1 * (1-x.21.1) + r.21 * (1-t1) * x.21.2 * (1-x.21.1) # Ica
(1 - r.21)* (1-g1) * (1-g6) * g8                # Oca
(1 - r.21)* (1-g1) * (1-g6) * (1-g8) * g9       + r.21 * (1-t1) * (1-x.21.2) # NVC
(1 - r.21)* (1-g1) * (1-g6) * (1-g8) * (1-g9)   # MISC
"


fopc <- "  
  # 5:AI1
  (1 - r.5)* g1 * g2 * g3 * g4                   # Aac
(1 - r.5)* g1 * g2 * g3 * (1-g4)               # Eac
(1 - r.5)* g1* g2 *(1-g3)                      # Iac
(1 - r.5)* g1* (1-g2) * g5                     # Oac
(1 - r.5)* g1 * (1-g2)* (1- g5)                # Aca
(1 - r.5)* (1-g1) * g6 * g7                    # Eca
(1 - r.5)* (1-g1) * g6 * (1-g7)                # Ica
(1 - r.5)* (1-g1) * (1-g6) * g8                # Oca
(1 - r.5)* (1-g1) * (1-g6) * (1-g8) * g9       + r.5 # NVC
(1 - r.5)* (1-g1) * (1-g6) * (1-g8) * (1-g9)   # MISC

# 21:II1
(1 - r.21)* g1 * g2 * g3 * g4                   # Aac
(1 - r.21)* g1 * g2 * g3 * (1-g4)               # Eac
(1 - r.21)* g1* g2 *(1-g3)                      # Iac
(1 - r.21)* g1* (1-g2) * g5                     # Oac
(1 - r.21)* g1 * (1-g2)* (1- g5)                # Aca
(1 - r.21)* (1-g1) * g6 * g7                    # Eca
(1 - r.21)* (1-g1) * g6 * (1-g7)                # Ica
(1 - r.21)* (1-g1) * (1-g6) * g8                # Oca
(1 - r.21)* (1-g1) * (1-g6) * (1-g8) * g9       + r.21 # NVC
(1 - r.21)* (1-g1) * (1-g6) * (1-g8) * (1-g9)   # MISC
"


phm <- "
# 5:AI1
(1 - r.5)* g1 * g2 * g3 * g4                   # Aac
(1 - r.5)* g1 * g2 * g3 * (1-g4)               # Eac
(1 - r.5)* g1* g2 *(1-g3)                      + r.5 * t1 + r.5 * (1-t1) * x.5.1 # Iac
(1 - r.5)* g1* (1-g2) * g5                     + r.5 * (1-t1) * (1-x.5.1) # Oac
(1 - r.5)* g1 * (1-g2)* (1- g5)                # Aca
(1 - r.5)* (1-g1) * g6 * g7                    # Eca
(1 - r.5)* (1-g1) * g6 * (1-g7)                # Ica
(1 - r.5)* (1-g1) * (1-g6) * g8                # Oca
(1 - r.5)* (1-g1) * (1-g6) * (1-g8) * g9       # NVC
(1 - r.5)* (1-g1) * (1-g6) * (1-g8) * (1-g9)   # MISC

# 21:II1
(1 - r.21)* g1 * g2 * g3 * g4                   # Aac
(1 - r.21)* g1 * g2 * g3 * (1-g4)               # Eac
(1 - r.21)* g1* g2 *(1-g3)                      + r.21 * t1 * x.21.1 +  r.21 * (1-t1) * x.21.2 * x.21.1 # Iac
(1 - r.21)* g1* (1-g2) * g5                     + r.21 * (1-t1) * (1-x.21.2) * x.21.3 # Oac
(1 - r.21)* g1 * (1-g2)* (1- g5)                # Aca
(1 - r.21)* (1-g1) * g6 * g7                    # Eca
(1 - r.21)* (1-g1) * g6 * (1-g7)                + r.21 * t1 * (1-x.21.1) + r.21 * (1-t1) * x.21.2 * (1-x.21.1) # Ica
(1 - r.21)* (1-g1) * (1-g6) * g8                + r.21 * (1-t1) * (1-x.21.2) * (1-x.21.3) # Oca
(1 - r.21)* (1-g1) * (1-g6) * (1-g8) * g9       # NVC
(1 - r.21)* (1-g1) * (1-g6) * (1-g8) * (1-g9)   # MISC
"



mmt_l <- make.mpt.cf(textConnection(mmt), treewise = TRUE)
## [[1]]
## [1] "r.5"   "t1"    "x.5.1" "3"     "7"     "x.5.2" "x.5.1" "3"     "7"     "9"     "g1"    "g2"    "g3"    "g4"   
## [15] "1"     "2"     "3"     "g5"    "4"     "5"     "g6"    "g7"    "6"     "7"     "g8"    "8"     "g9"    "9"    
## [29] "10"   

## [[2]]
## [1] "r.21"   "t1"     "x.21.1" "3"      "7"      "x.21.2" "x.21.1" "3"      "7"      "9"      "g1"     "g2"    
## [13] "g3"     "g4"     "1"      "2"      "3"      "g5"     "4"      "5"      "g6"     "g7"     "6"      "7"     
## [25] "g8"     "8"      "g9"     "9"      "1

lbmpt.to.mpt(mmt_l)
## Error in grepl("[[:alpha:]]", op) : node stack overflow

fopc_l <- make.mpt.cf(textConnection(fopc), treewise = TRUE)
lbmpt.to.mpt(fopc_l)
## Error in right[[2]] : subscript out of bounds
 
phm_l <-make.mpt.cf(textConnection(phm), treewise = TRUE)
lbmpt.to.mpt(phm_l)
## Error in .rendEq.rec(left[[2]]) : attempt to select less than one element
