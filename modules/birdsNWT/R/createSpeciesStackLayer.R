createSpeciesStackLayer <- function(modelList = sim$birdModels,
                                    pathData = dataPath(sim)){
  browser()
  message("Biomass data was simulated, using it for prediction")
  
}
# Check that the units are the same. For the bird models: species biomass layers are in units of 100 t/ha. Total biomass is in t/ha.
# In this function, if successionTables are NA (make sure of it in .inputObjects)
# Then it returns the StaticSpeciesLayers "https://drive.google.com/open?id=1cuvLiHkZxZnf0sEOIigOWBFyraOflpvl" with message
# otherwise, it needs to create the raster stack... I might need to steal some code from the predictDensities
# to this specific function (i.e. naming of layers).
