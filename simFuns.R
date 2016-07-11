simulate <- function(t, parms, type = "SI"){
	
	if(type == "SI"){
		yini.het <- findY(parms, type = "hetero")
		yini.app1 <- findY(parms, type = "homo")
		yini.app2 <- findY(parms, type = "lin.app")
		
		hetero.model <- hetero.model(parms)
		app.model1 <- approx.model(parms)
		app.model2 <- approx.model.lin(parms)
	}else if(type == "SIS1"){
		yini.het <- findY(parms, type = "hetero")
		yini.app1 <- findY(parms, type = "homo")
		yini.app2 <- findY(parms, type = "SIS.app")
		
		hetero.model <- hetero.model(parms)
		app.model1 <- approx.model(parms)
		app.model2 <- approx.model.SIS(parms)
	}else if(type == "SIS2"){
		yini.het <- findY(parms, type = "hetero")
		yini.app1 <- findY(parms, type = "SIS.app")
		yini.app2 <- findY(parms, type = "SIS.app2")
		
		hetero.model <- hetero.model(parms)
		app.model1 <- approx.model.SIS(parms)
		app.model2 <- approx.model.SIS2(parms)
	}else if(type == "SISalt"){
		yini.het <- findY(parms, type = "hetero")
		yini.app1 <- findY(parms, type = "SIS.app")
		yini.app2 <- findY(parms, type = "SIS.app2")
		
		hetero.model <- hetero.model(parms)
		app.model1 <- approx.model.SISalt(parms)
		app.model2 <- approx.model.SIS2(parms)
	}
	
	r.het <- rk(unlist(yini.het), func = hetero.model, parms = parms, time = t)
	r.app1 <- rk(unlist(yini.app1), func = app.model1, parms = parms, time = t)
	r.app2 <- rk(unlist(yini.app2), func = app.model2, parms = parms, time = t)
	
	return(list(r.het, r.app1, r.app2))
}