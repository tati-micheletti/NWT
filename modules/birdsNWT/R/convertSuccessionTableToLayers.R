convertSuccessionTableToLayers <- function(successionTables = sim$successionTables,
                                           pathData = dataPath(sim)){
  if (is.na(successionTables)){

    specieLayers <- reproducible::prepInputs(targetFile = "StaticSpeciesLayers.grd", 
                               url = "https://drive.google.com/open?id=1xvn26JEqcbFuCsnJMlaeUWfjbKLfWgNt",
                               destinationPath = pathData, fun = "raster::stack")
    return(specieLayers)
  } else {
    browser()
    # 1. Check which sp are in the succesion
    #For when LandR_Biomass is present
    # In this function, if successionTables are NA (make sure of it in .inputObjects)
    # Then it returns the StaticSpeciesLayers "https://drive.google.com/open?id=1cuvLiHkZxZnf0sEOIigOWBFyraOflpvl" with message
    # otherwise, it needs to create the raster stack... I might need to steal some code from the predictDensities
    # to this specific function (i.e. naming of layers).
  }
}