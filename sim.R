library("deSolve")
source("model.R")
source("distribution.R")
source("parameters.R")
source("functions.R")

tvec = seq(0,20,0.1)

gamma.parms <- expand(parms.skeleton, FUN = "gamma")

gamma.yini.het <- findY(gamma.parms, type = "hetero")

r.het <- rk(unlist(gamma.yini.het), func = hetero.model, parms = gamma.parms, time = tvec)
