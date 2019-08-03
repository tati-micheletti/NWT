makeAllPlots <- function(CSfolder, typeSim){
  library("usefun")
  library("LandR")
  library("reproducible")
  library("data.table")
  invisible(lapply(paste0("/mnt/data/Micheletti/NWT/posthocFunctions/", c("plotMaxAge.R", 
                                                                          "plotVegetationBiomass.R",
                                                                          "plotLeadingVegetationType.R",
                                                                          "totalBiomassPerSpecies.R",
                                                                          "plotBurnSummary.R",
                                                                          "disturbancePlotCaribou.R")), source))
  
  assign(x = paste0(typeSim, "leadVegType"), value = plotLeadingVegetationType(folderData = CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "maxAge"), value = plotMaxAge(folderData = CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "maxBiomass"), value = plotVegetationBiomass(folderData = CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "biomassPerSpecies"), value = totalBiomassPerSpecies(folderData = CSfolder, 
                                                                                  typeSim = typeSim))
  assign(x = paste0(typeSim, "biomassPerSpeciesProp"), value = totalBiomassPerSpecies(folderData = CSfolder, 
                                                                                      typeSim = typeSim, proportional = TRUE))
  assign(x = paste0(typeSim, "burnSumm"), value = plotBurnSummary(CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "disturbPlot"), value = disturbancePlotCaribou(CSfolder, typeSim = typeSim))
 toReturn <- usefun::grepMulti(ls(), patterns = typeSim)
 listToReturn <- lapply(toReturn, function(plt){
   r <- get(plt)
   })
 names(listToReturn) <- toReturn
  return(listToReturn)
}