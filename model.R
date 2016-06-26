hetero.model <- function(parameters){
	attach(parameters)
	if(rho > 0){
		gamma = 1
	}else{
		gamma = 0
	}
	detach(parameters)
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			S = yini[1:length(N.vec)]
			
			dS = rho * N0 -N.vec * S * beta * I - rho * S
			dI = sum(N.vec * S) * beta * I - gamma*I
			
			M1 = sum(N.vec * S)/sum(S)
			M2 = sum(N.vec^2 * S)/sum(S)
			M3 = sum(N.vec^3 * S)/sum(S)
			M4 = sum(N.vec^4 * S)/sum(S)
			kappa2 = M2/M1^2 -1
			kappa3 = M3/(M2 * M1) - 1
			kappa4 = M4/(M3 * M1) - 1
			
			return(list(c(dS,dI), kappa2 = kappa2, kappa3 = kappa3, kappa4 = kappa4, M = M1, totS = sum(S)))
		})
	}
	return(g)
}



approx.model <- function(parameters){
	attach(parameters)
	if(rho > 0){
		gamma = 1
	}else{
		gamma = 0
	}
	detach(parameters)
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			K = kappa2
			
			M = mean * (1-I)^K
			
			dS = rho * (1 - S) -M * S  * beta * I
			dI = M * S * beta * I - gamma*I
			
			return(list(c(dS,dI), kappa2 = K, M = M))
		})
	}
	return(g)
}

approx.model.r <- function(parameters){
	attach(parameters)
	
	if(rho > 0){
		gamma = 1
	}else{
		gamma = 0
	}
	
	constant = log((kappa2 + 1)/kappa2)
	r0 = kappa3/kappa2
	
	detach(parameters)
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			
			r = (r0-2) * S + 2
			
			M = mean * (1-I)^K
			
			dS = rho * (1 - S) -M * S  * beta * I
			dI = M * S * beta * I - gamma*I
			dK = -beta * I * M * (r-2) * (K + 1) * K
			
			return(list(c(dS,dI, dK), kappa2 = K, M = M, ratio = r))
		})
	}
	return(g)
}

approx.model.SIS <- function(parameters){
	attach(parameters)
	
	if(rho > 0){
		gamma = 1
	}else{
		gamma = 0
	}
	
	detach(parameters)
	
	g <- function(t, yini, parameters){
		with(as.list(c(yini,parameters)),{
			
			dS = rho * (1 - S) -M * S  * beta * I
			dI = M * S * beta * I - gamma * I
			dM = rho * (M_N - M)/S - beta * I * (phi - 1) * M^2
			dphi = phi * rho/S * ((phi_N - phi)*M_N^2 + phi*(M_N - M)^2)/(phi * M^2)
			
			return(list(c(dS,dI, dM, dphi), kappa2 = (phi-1)))
		})
	}
	return(g)
}
