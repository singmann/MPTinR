
# bug happend when restrictions do not map on paramaters in the model

data <- matrix (NA, nrow=3, ncol=9)
data [1, ] <- c (87,8,25,14,95,11,35,4,201)
data [2, ] <- c (74,16,45,23,76,36,28,17,225)
data [3, ] <- c (63,13,29,46,36,23,19,13,178)
rownames (data) <- c ("L(a)-I(s)response", "L(a)-I(a)response", "L(a)-I(a)response")
 
 

model <- "
D1*d1 + D1 *(1-d1)*a +(1-D1)*b*g
D1*(1-d1)*(1-a)+(1-D1)*b*(1-g)
(1-D1)*(1-b)

D2*(1-d2)*a +(1-D2)*b*g
D2*d2 + D2*(1-d2)*(1-a)+(1-D2)*b*(1-g)
(1-D2)*(1-b)

b*g
b*(1-g)
(1-b) "

fit.mpt(data, textConnection(model), textConnection("g = a"))

m6c <- fit.mpt(data, textConnection(model), textConnection("g = a"))
m5c <- fit.mpt(data, textConnection(model), textConnection("d1 = d2
g = a"))

# above should work, and worked
 
# below did not work because mapping of restricted paramaters to the model.

model2 <- "
D1.as*d1.as + D1.as *(1-d1.as)*a.as +(1-D1.as)*b.as*g.as
D1.as*(1-d1.as)*(1-a.as)+(1-D1.as)*b.as*(1-g.as)
(1-D1.as)*(1-b.as)

D2.as*(1-d2.as)*a.as +(1-D2.as)*b.as*g.as
D2.as*d2.as + D2.as*(1-d2.as)*(1-a.as)+(1-D2.as)*b.as*(1-g.as)
(1-D2.as)*(1-b.as)

b.as*g.as
b.as*(1-g.as)
(1-b.as)

D1.ab*d1.ab + D1.ab *(1-d1.ab)*a.ab +(1-D1.ab)*b.ab*g.ab
D1.ab*(1-d1.ab)*(1-a.ab)+(1-D1.ab)*b.ab*(1-g.ab)
(1-D1.ab)*(1-b.ab)

D2.ab*(1-d2.ab)*a.ab +(1-D2.ab)*b.ab*g.ab
D2.ab*d2.ab + D2.ab*(1-d2.ab)*(1-a.ab)+(1-D2.ab)*b.ab*(1-g.ab)
(1-D2.ab)*(1-b.ab)

b.ab*g.ab
b.ab*(1-g.ab)
(1-b.ab)

D1.aa*d1.aa + D1.aa *(1-d1.aa)*a.aa +(1-D1.aa)*b.aa*g.aa
D1.aa*(1-d1.aa)*(1-a.aa)+(1-D1.aa)*b.aa*(1-g.aa)
(1-D1.aa)*(1-b.aa)

D2.aa*(1-d2.aa)*a.aa +(1-D2.aa)*b.aa*g.aa
D2.aa*d2.aa + D2.aa*(1-d2.aa)*(1-a.aa)+(1-D2.aa)*b.aa*(1-g.aa)
(1-D2.aa)*(1-b.aa)

b.aa*g.aa
b.aa*(1-g.aa)
(1-b.aa) "

datan <- c(data[1,], data[2,], data[3,])

m2.1 <- fit.mpt(data, textConnection(model), textConnection("g = a
g.ab = a.ab
g.aa = a.aa"))

check.mpt(textConnection(model), textConnection("g.as = a.as
g.ab = a.ab
g.aa = a.aa"))
 


