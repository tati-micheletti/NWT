makeAllPlots <- function(CSfolder, typeSim){
  library("usefulFuns")
  library("LandR")
  library("reproducible")
  library("data.table")
  
  assign(x = paste0(typeSim, "leadVegType"), value = plotLeadingVegetationType(folderData = CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "maxAge"), value = plotMaxAge(folderData = CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "maxBiomass"), value = plotVegetationBiomass(folderData = CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "biomassPerSpecies"), value = totalBiomassPerSpecies(folderData = CSfolder, 
                                                                                  typeSim = typeSim))
  assign(x = paste0(typeSim, "biomassPerSpeciesProp"), value = totalBiomassPerSpecies(folderData = CSfolder, 
                                                                                      typeSim = typeSim, proportional = TRUE))
  assign(x = paste0(typeSim, "burnSumm"), value = plotBurnSummary(CSfolder, typeSim = typeSim))
  assign(x = paste0(typeSim, "disturbPlot"), value = disturbancePlotCaribou(CSfolder, typeSim = typeSim))
 toReturn <- usefulFuns::grepMulti(ls(), patterns = typeSim)
 listToReturn <- lapply(toReturn, function(plt){
   r <- get(plt)
   })
 names(listToReturn) <- toReturn
  return(listToReturn)
}
