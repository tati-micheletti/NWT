loadBiomassLayers <- function(scenarios, path, Run, years){
  scenarioNames <- unique(paste(tableCombinations[, vegetation], 
                            tableCombinations[, fire], sep = "_"))
  ras <- lapply(scenarioNames, function(scenario){
    yearsRas <- lapply(years, function(Y){
      name <- paste(scenario, Y, sep = "_")
      assign(name, raster(file.path(path, 
                                    scenario, 
                                    Run, 
                                    paste0("RAS_biomassYear",
                                           Y,
                                           ".tif"))))
      return(get(name))
    })
    names(yearsRas) <- paste0("year", years)
    return(yearsRas)
    })
  names(ras) <- scenarioNames
    return(ras)
}

