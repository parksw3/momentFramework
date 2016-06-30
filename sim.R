library("deSolve")
source("model.R")
source("parameters.R")
source("functions.R")
source("simFuns.R")

tvec <- seq(0,30,0.1)

parms.lnorm.SI <- expand(parms.SI, FUN = "lnorm")
parms.lnorm.SIS <- expand(parms.SIS, FUN = "lnorm")

SI.list.lnorm <- simulate(tvec, parms.lnorm.SI, type = "SI")
SIS.list.lnorm <- simulate(tvec, parms.lnorm.SIS, type = "SIS1")
SIS.list.lnorm2 <- simulate(tvec, parms.lnorm.SIS, type = "SIS2")

save("parms.lnorm.SI", "SI.list.lnorm",file="SI_sim.rda")
save("parms.lnorm.SIS", "SIS.list.lnorm",file="SIS_sim.rda")
save("parms.lnorm.SIS", "SIS.list.lnorm2",file="SIS_sim2.rda")

###

source("plotFunctions.R")
name <- c("Heterogeneous", "Approximation 1", "Approximation 2")
plotSim(SIS.list.lnorm, name)

###new code

parms.CV1 <- makeHetero(targetCV = 1, parms.skeleton2)
parms.CV2 <- makeHetero(targetCV = 2, parms.skeleton2)
parms.CV3 <- makeHetero(targetCV = 3, parms.skeleton2)

CV1.list <- simulate(tvec, parms.CV1)
