
# multicore et al.

sfInit(parallel=TRUE, cpus=3)


sav2 <- 
  set.seed(10)
  bmpt.fia(s = "ppppCpCCppCCCppCpCCppCCCppCCC", parameters = c(-0.5, -0.5, 3, 2, 5, 1, 5, 4, 2, 5, 1, 5, 1, 5), category = c(1,1,2,1,2,3,5,4,5,4,5,6,7,8,9), N = 1000, ineq0 = matrix(c(4,3),1,2))

set.seed(10)
bmpt.fia(s = "ppppCpCCppCCCppCpCCppCCCppCCC", parameters = c(-0.5, -0.5, 3, 2, 5, 1, 5, 4, 2, 5, 1, 5, 1, 5), category = c(1,1,2,1,2,3,5,4,5,4,5,6,7,8,9), N = 1000, ineq0 = matrix(c(4,3),1,2), split = 10)


bmpt.fia(s = "ppppCpCCppCCCppCpCCppCCCppCCC", parameters = c(-0.5, -0.5, 3, 2, 5, 1, 5, 4, 2, 5, 1, 5, 1, 5), category = c(1,1,2,1,2,3,5,4,5,4,5,6,7,8,9), N = 1000, ineq0 = matrix(c(4,3),1,2), multicore = TRUE)

system.time(fit.mpt(d.broeder, m.2htm, multicore = "individual"))


sfStop()