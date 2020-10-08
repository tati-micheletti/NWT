loadBiomassLayers <- function(scenarios, path, years){
  scenarioNames <- unique(paste(tableCombinations[, vegetation], 
                            tableCombinations[, fire], sep = "_"))
  ras <- lapply(scenarioNames, function(scenario){
    yearsRas <- lapply(years, function(Y){
      name <- paste(scenario, Y, sep = "_")
      RASfilePath <- file.path(path, scenario, paste0(scenario,"_RAS_biomassYear", Y, ".tif"))
      if (!file.exists(RASfilePath)){
        runsDir <- list.dirs(file.path(path, scenario), recursive = FALSE)
        allRuns <- raster::stack(lapply(X = runsDir, FUN = function(runPath){
          cohorDataList <- bringObjectTS(path = runPath, rastersNamePattern = c("cohortData", Y))
          names(cohorDataList) <- paste0(scenario, "Year", Y)
          pixelGroupList <- bringObjectTS(path = runPath, rastersNamePattern = c("pixelGroupMap", Y))
          names(pixelGroupList) <- paste0(scenario, "Year", Y)
            cohort <- cohorDataList[[1]] # To remove from list
            pixelGroup <- pixelGroupList[[1]] # To remove from list
            ch <- cohort[, list(sumBio = sum(B, na.rm = TRUE)), by = "pixelGroup"]
            maxBiomassPlot <- SpaDES.tools::rasterizeReduced(ch, pixelGroup, "sumBio", "pixelGroup")
          names(maxBiomassPlot) <- paste0(scenario,"BiomassYear", Y, "_", basename(runPath))
          return(maxBiomassPlot)
        }))
        # Calculate the average total biomass across all runs, per pixel
        runMean <- raster::calc(x = allRuns, fun = mean, na.rm = TRUE, 
                                filename = RASfilePath, format = "GTiff")
        return(RASfilePath)
      }
      assign(name, raster(RASfilePath))
      return(get(name))
    })
    names(yearsRas) <- paste0("year", years)
    return(yearsRas)
    })
  names(ras) <- scenarioNames
    return(ras)
}

