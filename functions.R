expand <- function(x,...){
	UseMethod("expand")
}


expand.parlist <- function(x, FUN,...){
	x <- within(x,
		{
			N.vec = seq(N.min,N.max,N.diff)
			distType = FUN
			N0 = calcDist(N.vec, mean, sd, distType)
			CV = sd/mean
		})
	return(x)
}

findY <- function(parms, type){
	with(as.list(c(parms)),{
		if(type == "homo"){
			yini <- list(
				S = 1 - iniI,
				I = iniI
			)
		}else if(type == "hetero"){
			yini <- list(
				S = N0 * (1 - iniI),
				I = iniI
			)
		}
		return(yini)
	})
}