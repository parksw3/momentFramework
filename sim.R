##experimenting stuff...

library("deSolve")
source("model.R")
source("distribution.R")
source("parameters.R")
source("functions.R")

tvec = seq(0,20,0.1)

parms.lnorm <- expand(parms.skeleton, FUN = "lnorm")

plot(parms.lnorm$N0)

yini.lnorm.het <- findY(parms.lnorm, type = "hetero")
yini.lnorm.hom <- findY(parms.lnorm, type = "homo")

approx.model.T <- approx.model(parms.lnorm, constantK = TRUE)
approx.model.F <- approx.model(parms.lnorm, constantK = FALSE)

r.lnorm.het <- rk(unlist(yini.lnorm.het), func = hetero.model, parms = parms.lnorm, time = tvec)
r.lnorm.app1 <- rk(unlist(yini.lnorm.hom), func = approx.model.T, parms = parms.lnorm, time = tvec)
r.lnorm.app2 <- rk(unlist(yini.lnorm.hom), func = approx.model.F, parms = parms.lnorm, time = tvec)

matplot(cbind(r.lnorm.het[,8002], r.lnorm.app1[,3], r.lnorm.app2[,3]), type = "l")
matplot(cbind(r.lnorm.het[,8003], r.lnorm.app1[,4], r.lnorm.app2[,4]),type = "l")
matplot(cbind(r.lnorm.het[,8004], r.lnorm.app1[,5], r.lnorm.app2[,5]),type = "l")

c <- parms.lnorm$N.vec 
S.mat <- r.lnorm.het[,2:(parms.lnorm$N.length+1)]
S <- rowSums(S.mat)
M1 <- rowSums(sweep(S.mat, 2, c, "*"))/rowSums(S.mat)
M2 <- rowSums(sweep(S.mat, 2, c^2, "*"))/rowSums(S.mat)
M3 <- rowSums(sweep(S.mat, 2, c^3, "*"))/rowSums(S.mat)
kappa2 <- M2/M1^2 -1
kappa3 <- M3/(M2 * M1) -1

kappa0 <- kappa2[1]
r <- kappa3[1]/kappa2[1]
constant <- log((kappa0 + 1)/kappa0)

kappa.approx <- S^(r-2)/(exp(constant) - S^(r-2))

matplot(cbind(kappa2, kappa.approx), type = "l")
