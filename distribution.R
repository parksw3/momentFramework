findGamma <- function(N, mean, sd){
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

## Examples

## N <- seq(0, 80, 0.001)
## a <- calcDist(N, 6, 4, "gamma")
## b <- calcDist(N, 6, 4, "lnorm")
## matplot(cbind(a,b), type = "l")
## sum(N * a)
## sum(N * b)
## sqrt(sum((N-sum(N*a))^2 * a))
## sqrt(sum((N-sum(N*b))^2 * b))