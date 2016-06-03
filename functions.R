expand <- function(x,...){
	UseMethod("expand")
}


expand.parlist <- function(x, FUN,...){
	x <- within(x,
		{
			N.vec = seq(N.min,N.max,N.diff)
			N.length = length(N.vec)
			distType = FUN
			N0 = calcDist(N.vec, mean, sd, distType)
			
			M1 = sum(N.vec * N0)
			M2 = sum(N.vec^2 * N0)
			M3 = sum(N.vec^3 * N0)
			kappa2 = M2/M1^2 -1
			kappa3 = M3/(M2 * M1) -1
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