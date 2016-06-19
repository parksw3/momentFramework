hetero.model <- function(t, yini, parameters){
	with(as.list(c(yini,parameters)),{
		S = yini[1:length(N.vec)]
		
		dS = mu * N0 -N.vec * S * beta * I - mu * S
		dI = sum(N.vec * S) * beta * I - mu*I
		
		M1 = sum(N.vec * S)/sum(S)
		M2 = sum(N.vec^2 * S)/sum(S)
		M3 = sum(N.vec^3 * S)/sum(S)
		M4 = sum(N.vec^4 * S)/sum(S)
		kappa2 = M2/M1^2 -1
		kappa3 = M3/(M2 * M1) - 1
		kappa4 = M4/(M3 * M1) - 1
		
		return(list(c(dS,dI), CV2 = kappa2, kappa3, kappa4, meanSus = M1))
	})
}

approx.model <- function(parameters){
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			K = kappa2
			
			sus = mean * (1-I)^K
			
			dS = mu * (1 - S) -sus * S  * beta * I
			dI = sus * S * beta * I - mu*I
			
			return(list(c(dS,dI), CV2 = K, meanSus = sus))
		})
	}
	return(g)
}

approx.model.r <- function(parameters){
	attach(parameters)
	
	constant = log((kappa2 + 1)/kappa2)
	r0 = kappa3/kappa2
	
	detach(parameters)
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			
			r = (r0-2) * S + 2
			
			sus = mean * (1-I)^K
			
			dS = mu * (1 - S) -sus * S  * beta * I
			dI = sus * S * beta * I - mu*I
			dK = -beta * I * sus * (r-2) * (K + 1) * K
			
			return(list(c(dS,dI, dK), CV2 = K, meanSus = sus, ratio = r))
		})
	}
	return(g)
}
