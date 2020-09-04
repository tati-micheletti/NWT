# This function gets the bird rasters needed for the posthocBirdsNWT
 
getBirdPredictedRasters <- function(parameters, predictedRastersFolder){
  
  birdRasters <- unlist(lapply(parameters[['posthocBirdsNWT']][['species']], function(species){ # future_lapply
    birdRasters <- lapply(parameters[['posthocBirdsNWT']][['years']], function(year){ # future_lapply
      birdYearRasters <- lapply(comparisons[['climateChange']], function(scenario){
        birdYearScenario <- lapply(parameters[['posthocBirdsNWT']][['runs']], function(Run){
          speciesScenario <- ifelse(scenario == "LandR_SCFM", "V4", "V6a")
          ras <- file.path(predictedRastersFolder, scenario, Run, 
                           paste0("birdPredictions", speciesScenario),
                           paste0(Run, "_", scenario, "predicted", 
                                  species, "Year", year, ".tif"))
          if (file.exists(ras)){
            message(paste("Raster for", species, year, scenario, speciesScenario, Run, 
                          "exists. Returning...", collapse = " "))
            ras <- raster(ras)
          } else {
            message("Apparently file doesn't exist. Are you sure the path is correct? ", ras)
            browser()
          }
          names(ras) <- paste0(species, year, scenario, speciesScenario, Run)
          return(ras)        
        })
      })
    })
  }))
  return(birdRasters)
}