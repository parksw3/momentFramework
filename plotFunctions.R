summarizeSim <- function(dat){
	df <- as.data.frame(dat)
	return(data.frame(
			time = df$time,
			Prevalence = df$I,
			kappa = df$CV2,
			susceptibility = df$meanSus
		))
}

meltSim <- function(simList, listName){
	sumList = lapply(simList, summarizeSim)
	names(sumList) = listName
	sumFrame = melt(sumList, id.vars = "time")
	sumFrame$simulations = factor(sumFrame$L1)
	return(sumFrame)
}

library("ggplot2"); theme_set(theme_classic())
library("reshape2")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plotSim <- function(simList, listName){
	sF = meltSim(simList, listName)
	return(
		ggplot(sF, aes(time, value, col = simulations)) +
		geom_line() +
		facet_wrap(~variable, scale = "free") +
		scale_colour_manual(values=cbPalette)
	)
}