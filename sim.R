library("deSolve")
source("model.R")
source("distribution.R")
source("parameters.R")
source("functions.R")

tvec = seq(0,30,0.1)

parms.lnorm <- expand(parms.skeleton, FUN = "lnorm")

yini.lnorm.het <- findY(parms.lnorm, type = "hetero")
yini.lnorm.hom <- findY(parms.lnorm, type = "homo")
yini.lnorm.app <- findY(parms.lnorm, type = "r.app")

approx.model.r <- approx.model.r(parms.lnorm)
approx.model <- approx.model(parms.lnorm)

r.lnorm.het <- rk(unlist(yini.lnorm.het), func = hetero.model, parms = parms.lnorm, time = tvec)
r.lnorm.app1 <- rk(unlist(yini.lnorm.app), func = approx.model.r, parms = parms.lnorm, time = tvec)
r.lnorm.app2 <- rk(unlist(yini.lnorm.hom), func = approx.model, parms = parms.lnorm, time = tvec)

dim(r.lnorm.het)
r.lnorm.het[1,]

M = r.lnorm.het[,8006]
k4 = r.lnorm.het[,8005]
k3 = r.lnorm.het[,8004]
k2 = r.lnorm.het[,8003]
I = r.lnorm.het[,8002]
r = k3/k2
lam = beta * I

save("parms.lnorm", "r.lnorm.het", "r.lnorm.app1", "r.lnorm.app2",file="sim1.rda")

###new code

parms.CV1 <- makeHetero(targetCV = 1, parms.skeleton2)
parms.CV2 <- makeHetero(targetCV = 2, parms.skeleton2)
parms.CV3 <- makeHetero(targetCV = 3, parms.skeleton2)

yini.CV1 <- findY(parms.CV1, type = "hetero")
yini.CV2 <- findY(parms.CV2, type = "hetero")
yini.CV3 <- findY(parms.CV3, type = "hetero")
yini.app1 <- findY(parms.CV1, type = "homo")
yini.app2 <- findY(parms.CV2, type = "homo")
yini.app3 <- findY(parms.CV3, type = "homo")

approx.model.CV1 <- approx.model(parms.CV1)
approx.model.CV2 <- approx.model(parms.CV2)
approx.model.CV3 <- approx.model(parms.CV3)
lin.approx.model.CV1 <- approx.model.r(parms.CV1)
lin.approx.model.CV2 <- approx.model.r(parms.CV2)
lin.approx.model.CV3 <- approx.model.r(parms.CV3)

r.het.CV1 <- rk(unlist(yini.CV1), func = hetero.model, parms = parms.CV1, time = tvec)
r.app.CV1 <- rk(unlist(yini.app1), func = approx.model.CV1, parms = parms.CV1, time = tvec)
r.het.CV2 <- rk(unlist(yini.CV2), func = hetero.model, parms = parms.CV2, time = tvec)
r.app.CV2 <- rk(unlist(yini.app2), func = approx.model.CV2, parms = parms.CV2, time = tvec)
r.het.CV3 <- rk(unlist(yini.CV3), func = hetero.model, parms = parms.CV3, time = tvec)
r.app.CV3 <- rk(unlist(yini.app3), func = approx.model.CV3, parms = parms.CV3, time = tvec)

###


