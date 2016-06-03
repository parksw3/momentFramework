hetero.model <- function(t, yini, parameters){
	with(as.list(c(yini,parameters)),{
		S = yini[1:length(N.vec)]
		
		dS = mu * N0 -N.vec * S * beta * I - mu * S
		dI = sum(N.vec * S) * beta * I - mu*I
		
		return(list(c(dS,dI)))
	})
}