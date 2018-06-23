# Quad Model in R using MPTinR

model.quad1<-"

# Condition 1, first correct then incorrect
# p(correct | alcohol, practice)
Ddr + G * (1-Ddr)
(1-G) * (1-Ddr)

# Condition 2, first correct then incorrect
# p(correct | non-alcohol, practice)
Ddr+(1-G)*(1-Ddr)
G*(1-Ddr)

# Condition 3, first correct then incorrect
# p(correct | approach, practice)
Datt+G*(1-Datt) 
(1-G)*(1-Datt)

# Condition 4, first correct then incorrect
# p(correct | avoid, practice)
Datt+(1-G)*(1-Datt) 
(1-Datt)*G

# Condition 5, first correct then incorrect
# p(correct | alcohol, congruent)
AC*Ddr*OB+AC*Ddr*(1-OB)+AC*(1-Ddr)+(1-AC)*Ddr+(1-AC)*(1-Ddr)*G 
(1-AC)*(1-Ddr)*(1-G)

# Condition 6, first correct then incorrect
# p(correct | non-alcohol, congruent)
AC*Ddr*OB+AC*Ddr*(1-OB)+AC*(1-Ddr)+(1-AC)*Ddr+(1-AC)*(1-Ddr)*(1-G) 
(1-AC)*(1-Ddr)*G

# Condition 7, first correct then incorrect
# p(correct | approach, congruent)
AC*Datt*OB+AC*Datt*(1-OB)+AC*(1-Datt)+(1-AC)*Datt+(1-AC)*(1-Datt)*G 
(1-AC)*(1-Datt)*(1-G)

# Condition 8, first correct then incorrect
# p(correct | avoid, congruent)
AC*Datt*OB+AC*Datt*(1-OB)+AC*(1-Datt)+(1-AC)*Datt+(1-AC)*(1-Datt)*(1-G) 
(1-AC)*(1-Datt)*G

# Condition 9, first correct then incorrect
# p(correct | alcohol, incongruent)
AC*Ddr*OB+(1-AC)*Ddr+(1-AC)*(1-Ddr)*G 
AC*Ddr*(1-OB)+AC*(1-Ddr)+(1-AC)*(1-Ddr)*(1-G)

# Condition 10, first correct then incorrect
# p(correct | non-alcohol, incongruent)
AC*Ddr*OB+(1-AC)*Ddr+(1-AC)*(1-Ddr)*(1-G) 
AC*Ddr*(1-OB)+AC*(1-Ddr)+(1-AC)*(1-Ddr)*G

# Condition 11, first correct then incorrect
# p(correct | approach, incongruent)
AC*Datt*OB+(1-AC)*Datt+(1-AC)*(1-Datt)*G 
AC*Datt*(1-OB)+AC*(1-Datt)+(1-AC)*(1-Datt)*(1-G)

# Condition 12, first correct then incorrect
# p(correct | avoid, incongruent)
AC*Datt*OB+(1-AC)*Datt+(1-AC)*(1-Datt)*(1-G) 
AC*Datt*(1-OB)+AC*(1-Datt)+(1-AC)*(1-Datt)*G

"

check.mpt(textConnection(model.quad1)) # This works all fine
sim.dat<-GetProbMPT(c(.3,.4,.5,.6,.7)) # Again take simulated data

(sim.dat <- gen.predictions(c(.3,.4,.5,.6,.7), textConnection(model.quad1)))

fit.mpt(sim.dat,textConnection(model.quad1))


        