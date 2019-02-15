convertSuccessionTableToLayers <- function(successionTables = sim$successionTables,
                                           modelList = sim$birdModels,
                                           pathData = dataPath(sim)){
  if (is.na(successionTables)){
    message("Biomass data not simulated, using original dataset as test")
    specieLayers <- reproducible::prepInputs(targetFile = "StaticSpeciesLayers.grd", 
                               url = "https://drive.google.com/open?id=1xvn26JEqcbFuCsnJMlaeUWfjbKLfWgNt",
                               alsoExtract = "StaticSpeciesLayers.gri", 
                               destinationPath = pathData, fun = "raster::stack")
    return(specieLayers)
  } else {
    browser()
    # 1. Check which sp are in the succesion
    #For when LandR_Biomass is present
    # Check that the units are the same. For the bird models: species biomass layers are in units of 100 t/ha. Total biomass is in t/ha.
    # In this function, if successionTables are NA (make sure of it in .inputObjects)
    # Then it returns the StaticSpeciesLayers "https://drive.google.com/open?id=1cuvLiHkZxZnf0sEOIigOWBFyraOflpvl" with message
    # otherwise, it needs to create the raster stack... I might need to steal some code from the predictDensities
    # to this specific function (i.e. naming of layers).
  }
}