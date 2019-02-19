loadTestSpeciesLayers <- function(successionTables = sim$successionTables,
                                           modelList = sim$birdModels,
                                           pathData = dataPath(sim)){
    message("Biomass data not simulated, using original dataset as test")
  specieLayers <- reproducible::prepInputs(targetFile = "StaticSpeciesLayers.grd",
                                           archive = "StaticSpeciesLayers.zip",
                                           alsoExtract = "similar", purge = 7,
                               url = "https://drive.google.com/open?id=1QiwMJpbQYeBH5ifNZDEXe04bHmn-w4Oc",
                               destinationPath = pathData, fun = "raster::stack")
    return(specieLayers)
}