

dataProb <- structure(list(V1 = c(30, 60, 20, 30, 40, 50, 100, 30, 40, 40, 
30, 30, 30, 20, 30, 10, 10, 30, 20, 40, 50, 30, 20, 50, 20, 100
), V2 = c(70, 40, 80, 70, 60, 50, 0, 70, 60, 60, 70, 70, 70, 
80, 70, 90, 90, 70, 80, 60, 50, 70, 80, 50, 80, 0), V3 = c(50, 
90, 40, 40, 80, 50, 100, 40, 60, 80, 60, 50, 60, 50, 40, 40, 
40, 70, 40, 60, 60, 20, 40, 60, 40, 100), V4 = c(50, 10, 60, 
60, 20, 50, 0, 60, 40, 20, 40, 50, 40, 50, 60, 60, 60, 30, 60, 
40, 40, 80, 60, 40, 60, 0)), .Names = c("V1", "V2", "V3", "V4"
), row.names = c(NA, -26L), class = "data.frame")

# My Model
Modello1<- "
U*(1-No)
(1-U) + (U*No)

U*(1-No) + (U*No)
(1-U)"

Modello2<- "
U*(1-No)
(1-U) + U*No

U*(1-No) + U*No
(1-U)"




# My Restriction
restri<-"U = 0.56"

check.mpt(textConnection(Modello1))


# Model fit
fit.mpt(dataProb, textConnection(Modello1), textConnection(restri) )

fit.mpt(dataProb, textConnection(Modello1), textConnection(restri), fia = 100000 )

options(error = recover)

