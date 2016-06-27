simulate <- function(t, parms, SIS = FALSE){
	
	yini.het <- findY(parms, type = "hetero")
	yini.hom <- findY(parms, type = "homo")
	
	hetero.model <- hetero.model(parms)
	constantK.model <- approx.model(parms)
	
	if(SIS){
		yini.app <- findY(parms, type = "SIS.app")
		approx.model <- approx.model.SIS(parms)
	}else{
		yini.app <- findY(parms, type = "lin.app")
		approx.model <- approx.model.lin(parms)
	}
	
	r.het <- rk(unlist(yini.het), func = hetero.model, parms = parms, time = t)
	r.app1 <- rk(unlist(yini.hom), func = constantK.model, parms = parms, time = t)
	r.app2 <- rk(unlist(yini.app), func = approx.model, parms = parms, time = t)
	
	return(list(r.het, r.app1, r.app2))
}