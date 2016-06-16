######################################
#	Old code. Don't need these for now.#
######################################

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
