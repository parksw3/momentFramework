library("deSolve")
source("model.R")
source("parameters.R")
source("functions.R")
source("simFuns.R")

tvec <- seq(0,30,0.1)

parms.lnorm.SI <- expand(parms.SI, FUN = "lnorm")
parms.lnorm.SIS <- expand(parms.SIS, FUN = "lnorm")

SI.list.lnorm <- simulate(tvec, parms.lnorm.SI)
SIS.list.lnorm <- simulate(tvec, parms.lnorm.SIS, SIS = TRUE)

save("parms.lnorm.SI", "SI.list.lnorm",file="SI_sim.rda")
save("parms.lnorm.SIS", "SIS.list.lnorm",file="SIS_sim.rda")

###new code

parms.CV1 <- makeHetero(targetCV = 1, parms.skeleton2)
parms.CV2 <- makeHetero(targetCV = 2, parms.skeleton2)
parms.CV3 <- makeHetero(targetCV = 3, parms.skeleton2)

CV1.list <- simulate(tvec, parms.CV1)
