createSpeciesStackLayer <- function(modelList = sim$birdModels,
                                    simulatedBiomassMap = sim$simulatedBiomassMap,
                                    cohortData = sim$cohortData, # Should also have age
                                    sppEquivCol = sim$sppEquivCol,
                                    pixelGroupMap = sim$pixelGroupMap,
                                    pathData = dataPath(sim)){
  browser()
  message("Biomass data was simulated, using it for prediction")
  predictors <- modelList[[1]]$gbm.call$predictor.names
  biomassLayerName <- predictors[grepl(x = predictors, pattern = "Biomass")]
  speciesLayerNames <- predictors[pmatch(sim$sppEquivCol, predictors)]
  biomassLayerName <- predictors[grep(x = predictors, pattern = "Biomass")]
}

# Species layers have to be
# Check that the units are the same. For the bird models: species biomass layers are in units of 100 t/ha. Total biomass is in t/ha.
# In this function, if successionTables are NA (make sure of it in .inputObjects)
# Then it returns the StaticSpeciesLayers "https://drive.google.com/open?id=1cuvLiHkZxZnf0sEOIigOWBFyraOflpvl" with message
# otherwise, it needs to create the raster stack... I might need to steal some code from the predictDensities
# to this specific function (i.e. naming of layers).
