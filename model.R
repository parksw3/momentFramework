hetero.model <- function(t, yini, parameters){
	with(as.list(c(yini,parameters)),{
		S = yini[1:length(N.vec)]
		
		dS = mu * N0 -c * S * beta * I - mu * S
		dI = sum(c * S) * beta * I - mu*I
		
		S0 = sum(S)
		S1 = sum(c * S)
		S2 = sum(c^2 * S)
		S3 = sum(c^3 * S)
		M0 = 1
		M1 = S1/S0
		M2 = S2/S0
		M3 = S3/S0
		
		kappa2 = M2/(M1^2) - 1
		kappa3 = M3/(M2*M1) - 1
		
		return(list(c(dS,dI), N = sum(S) + I, kappa2, kappa3))
	})
}

tvec = seq(0,20,0.1)

infD = N.vec * iniD/sum(N.vec * iniD)

iniI = 0.0001

yini.hetero <- list(
	S = iniD/sum(iniD) *(1 - iniI), I = iniI
)

r.het <- rk(unlist(yini.hetero), func = hetero.model, parms = base.parms, time = tvec)

