
# fitting the data in Hoffman, Bein, & Maril (2012) Psych Science to an SDT

exp1 <- c(.12, .40, .31, .17, .14, .50, .28, .08, .21, .43, .29, .07) * 25 * 32
exp2 <- c(.10, .47, .29, .14, .14, .55, .24, .07, .19, .49, .25, .07) * 25 * 30

sdt.model1 <- "testfiles/sdt.ex1.model"

check.mpt(sdt.model1)

fit.model(exp1, sdt.model1, lower.bound = c(-Inf, 0,0, -Inf, -Inf, 0.1, 0.1), upper.bound = Inf)

