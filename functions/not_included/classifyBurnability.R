classifyBurnability <- function(cohortData, pixelGroupMap, pixelsToSubset = NULL){
  
  # Function to reclassify
  reclassCohortData <- function(cohortData, reclassTable){
    newCohortData <- reclassTable[cohortData, on = "speciesCode"][age < 15, burnClass := "class1"]
    return(newCohortData)
  }
  
  if (!is.null(pixelsToSubset)){
    # Get the pixel groups for the fire polygons we have from 1991 - 2017
    firePixelGroup <- pixelGroupMap[pixelsToSubset]
    # Reduce cohortData to these pixels
    cohortData <- cohortData[pixelGroup %in% firePixelGroup, ]
  }
  
  setDT(cohortData) # Bring the cohortData table
  spCode <- c('Pice_Mar', 'Pice_Gla', 'Lari_Lar', 'Betu_Pap', 'Popu_Tre', 'Pinu_Ban') # TODO Make it flexible!
  reclassTable <- data.table(speciesCode = spCode, burnClass = c("class3", "class3", "class3", "class2", "class2", "class4"))
  
  # Classify the burnClasses
  cohortData <- reclassCohortData(cohortData = cohortData, reclassTable = reclassTable)
  # Calculate proportional biomass (it might or might now be present already in cohortData)
  cohortData[, totalBiomass := sum(B), by = "pixelGroup"]
  cohortData[, propBiomass := B/totalBiomass, by = "pixelGroup"]
  # Calculate proportional biomass per class
  cohortData[, propBurnClass := sum(propBiomass), by = c("pixelGroup", "burnClass")]
  return(cohortData)
}