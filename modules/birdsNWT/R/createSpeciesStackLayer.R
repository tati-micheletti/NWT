createSpeciesStackLayer <- function(modelList = sim$birdModels,
                                    simulatedBiomassMap = sim$simulatedBiomassMap,
                                    cohortData = sim$cohortData, # Should also have age
                                    sppEquiv = sim$sppEquiv,
                                    staticLayers = sim$staticLayers,
                                    pixelGroupMap = sim$pixelGroupMap,
                                    pathData = dataPath(sim)){
reproducible::Require("data.table")
reproducible::Require("plyr")
reproducible::Require("raster")
  
  message("Biomass data was simulated, using it for prediction")
  
  # Create layer names based on the model
  predictors <- modelList[[1]]$gbm.call$predictor.names
  speciesNames <- unique(na.omit(sppEquiv[,NWT]))
  speciesLayerNames <- rbindlist(lapply(X = speciesNames, FUN = function(sp){
    speciesLayerNames <- data.table::data.table(modelLayer = predictors[grepl(sp, predictors)], 
                                                speciesName = sp)
  })
  )
  # Iterate through species and for each species, plot the B in the `pixelGroupMap`
  speciesRasters <- lapply(X = speciesNames, FUN = function(sp){
    subsCohort <- cohortData[speciesCode == sp, ]
    if (NROW(subsCohort) == 0){
      zeroedMap <- pixelGroupMap
      vals <- getValues(x = zeroedMap)
      vals[!is.na(vals)] <- 0
      zeroedMap <- setValues(x = zeroedMap, values = vals)
      assign(x = sp, value = zeroedMap)
      names(zeroedMap) <- speciesLayerNames[speciesName == sp, modelLayer]
      return(zeroedMap)
    } else {
      valsCoho <- data.table(pixelID = 1:ncell(pixelGroupMap), 
                             pixelGroup = getValues(x = pixelGroupMap))
      setkey(valsCoho$pixelGroup)
      newCohoVals <- plyr::join(x = valsCoho, subsCohort[, list(sumBiomass=sum(B)), by = c("speciesCode", "pixelGroup")])
      spMap <- setValues(x = pixelGroupMap, values = newCohoVals$sumBiomass)
      assign(x = sp, value = spMap)
      names(spMap) <- speciesLayerNames[speciesName == sp, modelLayer]
      return(spMap)
    }
  })
  
  # Rename biomass
  biomassLayerName <- predictors[grepl(x = predictors, pattern = "Biomass")]
  biomass <- simulatedBiomassMap
  names(biomass) <- biomassLayerName
  
  # Creat age map
  ageLayerName <- predictors[grepl(x = predictors, pattern = "Age")]
  ageMap <- pixelGroupMap
  valsAge <- data.table(pixelID = 1: ncell(ageMap), pixelGroup = getValues(x = pixelGroupMap))
  newAgeVals <- plyr::join(x = valsAge, unique(cohortData[,c("pixelGroup", "age")]))
  newAgeVals <- setDT(newAgeVals)[, .SD[1], by = .(pixelID)] # [ FIX ] Need to see which Age to use. Now I exclude the second on. 
  newAgeMap <- setValues(x = ageMap, values = newAgeVals$age)
  assign(x = ageLayerName, value = newAgeMap)
  names(newAgeMap) <- ageLayerName
  
  speciesStack <- raster::stack(speciesRasters) %>%
    raster::stack(biomass) %>%
    raster::stack(newAgeMap)
  
  # Make sure that species that were not modeled by LandR still have a raster
  layersAvailable <- c(names(staticLayers), names(speciesStack))
  missingLayersNames <- setdiff(predictors, layersAvailable)
  missingLayers <- lapply(X = missingLayersNames, FUN = function(miss){
    zeroedMap <- pixelGroupMap
    vals <- getValues(x = zeroedMap)
    vals[!is.na(vals)] <- 0
    zeroedMap <- setValues(x = zeroedMap, values = vals)
    names(zeroedMap) <- miss
    return(zeroedMap)
  })
  
  finalStk <- raster::stack(missingLayers) %>%
    raster::stack(speciesStack)

  return(finalStk)
}
