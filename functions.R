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
		M_N = M1
		phi_N = kappa2 + 1
	})
	return(x)
}

findGamma <- function(N, mean, sd){
	kappa = sd^2/mean^2
	k = 1/kappa
	theta = sd^2/mean
	k = mean/theta
	dist = dgamma(N, shape = k, scale = theta)
	return(dist)
}

findLnorm <-function(N, mean, sd){
	kappa = sd^2/mean^2
	sigma = sqrt(log(kappa+1))
	mu = log(mean)- sigma^2/2
	dist = dlnorm(N, meanlog = mu, sdlog = sigma)
}

calcDist <- function(N, mean, sd, FUN = "gamma"){
	if(FUN == "gamma"){
		func = findGamma	
	}else if(FUN == "lnorm"){
		func = findLnorm
	}
	finalDist <- func(N, mean,sd)/sum(func(N, mean,sd))
	return(finalDist)
}

f1 <- function(x){
	return(1/(1+x)^4)
}

f2 <- function(x){
	return(1/(1+x^2))
}

f3 <- function(x){
	return(1/(1+x))
}

findCV <- function(N, distribution){
	M1 <- sum(N * distribution)/sum(distribution)
	M2 <- sum(N^2 * distribution)/sum(distribution)
	return(M2/M1^2 - 1)
}

newMean <- function(Amax, FUN, targetCV){
	a <- seq(from = 0, to = Amax, length.out = 500) + Amax/998
	N <- FUN(a)
	CV <- findCV(a, N)
	if(CV < targetCV){
		interval <- c(0.000001,1)
	}else{
		interval <- c(1,1000)
	}
	B <- uniroot(function(x)
		findCV(a,FUN(a/x)) - targetCV
		,interval)$root
	N2 <- FUN(a/B)
	sL <- list(mean = sum(a*N2)/sum(N2), B = B)
	return(sL)
}

makeHetero <- function(targetCV, x){
	with(as.list(c(x)),{
		targetMean = mean
		FUN = paste0("f", targetCV)
		interval = c(2,40)
		
		x <- within(x,{
			Amax = uniroot(function(y)
				newMean(y, get(FUN), targetCV)$mean - targetMean ,interval)$root
			B = newMean(Amax, get(FUN), targetCV)$B
			N.length = 500
			N.vec = seq(from = 0, to = Amax, length.out = N.length) + Amax/998
			k = 1/sum(get(FUN)(N.vec/B))
			N0 = k * get(FUN)(N.vec/B)
			
			M1 = sum(N.vec * N0)
			M2 = sum(N.vec^2 * N0)
			M3 = sum(N.vec^3 * N0)
			kappa2 = M2/M1^2 -1
			kappa3 = M3/(M2 * M1) -1
			M_N = M1
			phi_N = kappa2 + 1
		})
		return(x)		
	})
	
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
		}else if(type == "lin.app"){
			yini <- list(
				S = 1 - iniI,
				I = iniI,
				K = kappa2
			)
		}else if(type == "SIS.app"){
			yini <- list(
				S = 1 - iniI,
				I = iniI,
				M = mean,
				phi = kappa2 + 1
			)
		}else if(type == "SIS.app2"){
			yini <- list(
				S = 1 - iniI,
				I = iniI,
				M = mean
			)
		}
		
		return(yini)
	})
}