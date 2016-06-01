findGamma <- function(N, mean, sd){
	theta = mean/sd^2
	k = mean/theta
	dist = dgamma(N, shape = k, scale = theta)
	return(dist)
}

findLnorm <-function(N, mean, sd){
	kappa = sd^2/mean
	sigma = sqrt(log(kappa) + 1)
	mu = log(mean)- sigma^2/2
	dist = dlnorm(N, meanlog = mu, sdlog = sigma)
}

calcDist <- function(mean, sd, FUN = "gamma", parms){
	with(as.list(c(parms)),{
		if(FUN == "gamma"){
			func = findGamma	
		}else if(FUN == "lnorm"){
			func = findLnorm
		}
		finalDist <- func(N,vec, mean,sd)/sum(func(N.vec, mean,sd))
		return(finalDist)
	})
}
