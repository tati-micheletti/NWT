sim <- get("sim", envir = whereInStack("sim"))
MOD <- sim$birdModels$ALFL

summAllVarsFromMod <- rbindlist(lapply(variables, function(VAR){
  sumMod <- summary(MOD$gbm.call$dataframe[[VAR]])
  a <- data.table(variable = VAR, 
             Min = as.numeric(sumMod["Min."]),
             Max = as.numeric(sumMod["Max."]),
             origin = "Model")
return(a)
}))

summAllVarsFromLays <- data.table(variable = variables, 
                                  Min = raster::minValue(ensembleStack[[1]]),
                                  Max = raster::maxValue(ensembleStack[[1]]),
                                  origin = "Layers")

tb <- rbind(summAllVarsFromMod, summAllVarsFromLays)
tbFin <- rbindlist(lapply(variables, function(VAR){
  tb[variable == VAR]
return(tb[variable == VAR])
}))

