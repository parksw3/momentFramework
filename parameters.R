parms.skeleton <- list(
	N.diff = 0.01,
	N.min = 0.01,
	N.max = 80,
	mean = 4,
	sd = 5,
	beta = 0.2,
	rho = 0.4,
	iniI = 0.001
)

class(parms.skeleton) <- c("list", "parlist")

parms.skeleton2 <- list(
	mean = 1,
	beta = 0.8,
	rho = 0,
	iniI = 0.0001
)