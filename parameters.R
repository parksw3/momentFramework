parms.SI <- list(
	N.diff = 0.01,
	N.min = 0.01,
	N.max = 80,
	mean = 3,
	sd = 2,
	beta = 0.2,
	rho = 0,
	iniI = 0.001
)

class(parms.SI) <- c("list", "parlist")

parms.SIS <- list(
	N.diff = 0.01,
	N.min = 0.01,
	N.max = 80,
	mean = 3,
	sd = 2,
	beta = 0.7,
	rho = 0.4,
	iniI = 0.001
)

class(parms.SIS) <- c("list", "parlist")

parms.highCV <- list(
	mean = 3,
	beta = 0.7,
	rho = 0.4,
	iniI = 0.001
)