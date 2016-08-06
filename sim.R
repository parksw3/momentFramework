library("deSolve")
source("model.R")
source("parameters.R")
source("functions.R")
source("simFuns.R")

tvec <- seq(0,30,0.1)

parms.lnorm.SI <- expand(parms.SI, FUN = "lnorm")
parms.lnorm.SIS <- expand(parms.SIS, FUN = "gamma")

SI.list.lnorm <- simulate(tvec, parms.lnorm.SI, type = "SI")
SIS.list.lnorm <- simulate(tvec, parms.lnorm.SIS, type = "SIS1")
SIS.list.lnorm2 <- simulate(tvec, parms.lnorm.SIS, type = "SIS2")
SIS.list.lnorm3 <- simulate(tvec, parms.lnorm.SIS, type = "SISalt")

save("parms.lnorm.SI", "SI.list.lnorm",file="SI_sim.rda")
save("parms.lnorm.SIS", "SIS.list.lnorm",file="SIS_sim.rda")
save("parms.lnorm.SIS", "SIS.list.lnorm2",file="SIS_sim2.rda")

###

source("plotFunctions.R")
name <- c("Heterogeneous", "Approximation 1", "Approximation 2")
plotSim(SIS.list.lnorm2, name)

###new code

parms.CV1 <- makeHetero(targetCV = 1, parms.highCV)
parms.CV2 <- makeHetero(targetCV = 2, parms.highCV)
parms.CV3 <- makeHetero(targetCV = 3, parms.highCV)

SIS.list.CV1 <- simulate(tvec, parms.CV1, type = "SIS2")
SIS.list.CV2 <- simulate(tvec, parms.CV2, type = "SIS2")
SIS.list.CV3 <- simulate(tvec, parms.CV3, type = "SIS2")

save("parms.CV1", "SIS.list.CV1",file="SIS_simCV1.rda")
save("parms.CV2", "SIS.list.CV2",file="SIS_simCV2.rda")
save("parms.CV3", "SIS.list.CV3",file="SIS_simCV3.rda")
