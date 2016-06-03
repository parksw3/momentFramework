hetero.model <- function(t, yini, parameters){
	with(as.list(c(yini,parameters)),{
		S = yini[1:length(N.vec)]
		
		dS = mu * N0 -N.vec * S * beta * I - mu * S
		dI = sum(N.vec * S) * beta * I - mu*I
		
		M1 = sum(N.vec * S)/sum(S)
		M2 = sum(N.vec^2 * S)/sum(S)
		M3 = sum(N.vec^3 * S)/sum(S)
		kappa2 = M2/M1^2 -1
		
		return(list(c(dS,dI), CV2 = kappa2, meanSus = M1))
	})
}

approx.model <- function(parameters, constantK = TRUE){
	attach(parameters)
	
	constant = log((kappa2 + 1)/kappa2)
	r0 = kappa3/kappa2
	
	detach(parameters)
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			 if(constantK == TRUE){
				K = kappa2
			}else{
				r = (r0-2) * S + 2
				K = S^(r-2)/(exp(constant) - S^(r-2))
			}
			
			sus = mean * (1-I)^K
			
			dS = mu * (1 - S) -sus * S  * beta * I
			dI = sus * S * beta * I - mu*I
			
			return(list(c(dS,dI), CV2 = K, meanSus = sus))
		})
	}
	return(g)
}


