library("deSolve")
source("model.R")
source("distribution.R")
source("parameters.R")
source("functions.R")

tvec = seq(0,30,0.1)

parms.lnorm <- expand(parms.skeleton, FUN = "lnorm")

plot(parms.lnorm$N0, type = "l")

yini.lnorm.het <- findY(parms.lnorm, type = "hetero")
yini.lnorm.hom <- findY(parms.lnorm, type = "homo")

approx.model.T <- approx.model(parms.lnorm, constantK = TRUE)
approx.model.F <- approx.model(parms.lnorm, constantK = FALSE)

r.lnorm.het <- rk(unlist(yini.lnorm.het), func = hetero.model, parms = parms.lnorm, time = tvec)
r.lnorm.app1 <- rk(unlist(yini.lnorm.hom), func = approx.model.T, parms = parms.lnorm, time = tvec)
r.lnorm.app2 <- rk(unlist(yini.lnorm.hom), func = approx.model.F, parms = parms.lnorm, time = tvec)

save("parms.lnorm", "r.lnorm.het", "r.lnorm.app1", "r.lnorm.app2",file="sim1.rda")