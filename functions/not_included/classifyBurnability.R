classifyBurnability <- function(cohortData, pixelGroupMap, pixelsToSubset = NULL){
  # 
  
  # NEEDS TO BE REDONE BASED ON !runMe.R!
  
  # # Function to reclassify
  # reclassCohortData <- function(cohortData, reclassTable){
  #   newCohortData <- cohortData[reclassTable, on = "speciesCode"]
  #   newCohortData[age < 15, burnClass := "class1"]
  #   
  #   # Assertion
  #   testthat::expect_true(all(newCohortData$pixelGroup %in% firePixelGroup))
  #   
  #   return(newCohortData)
  # }
  # 
  # if (!is.null(pixelsToSubset)){
  #   # Get the pixel groups for the fire polygons for the given years
  #   # As fires happened in non-forest places as well, we have NA's here
  #   # (i.e. a lot of NA's @ 40-50%)
  #   firePixelGroup <- pixelGroupMap[pixelsToSubset]
  #   # Reduce cohortData to these pixels
  #   # any(is.na(cohortData$pixelGroup)) == FALSE : No NA's in pixelGroups
  #   cohortData <- cohortData[pixelGroup %in% firePixelGroup, ]
  #   # All cohortData's pixelGroups are in the firePixelGroup: 
  #         # We just subsetted the cohort data to the firePixelGroups
  #   # Not all firePixelGroup  are in the cohortData's pixelGroups: 
  #         # We have several fires that did NOT happen in forests, so do not have a 
  #         # correspondent pixelGroup
  #   # Browse[1]> all(cohortData$pixelGroup %in% firePixelGroup)
  #   # [1] TRUE
  #   # Browse[1]> all(firePixelGroup %in% cohortData$pixelGroup)
  #   # [1] FALSE
  # 
  # }
  # 
  # spCode <- c('Pice_Mar', 'Pice_Gla', 'Lari_Lar', 'Betu_Pap', 'Popu_Tre', 'Pinu_Ban') # TODO Make it flexible!
  # reclassTable <- data.table(speciesCode = spCode, burnClass = c("class3", "class3", "class3", "class2", "class2", "class4"))
  # 
  # # Classify the burnClasses
  # cohortData <- reclassCohortData(cohortData = cohortData, reclassTable = reclassTable)
  # 
  # # Calculate proportional biomass (it might or might now be present already in cohortData)
  # cohortData[, totalBiomass := sum(B, na.rm = TRUE), by = "pixelGroup"]
  # cohortData[, propBiomass := B/totalBiomass, by = "pixelGroup"]
  # # Calculate proportional biomass per class
  # cohortData[, propBurnClass := sum(propBiomass, na.rm = TRUE), by = c("pixelGroup", "burnClass")]
  # cohortData[totalBiomass == 0 & age == 0, c("propBiomass", "propBurnClass") := 1]
  # 
  # # Assertion: we don't have any more NA's
  # testthat::expect_true(NROW(cohortData) == NROW(na.omit(cohortData)))
  # 
  return(cohortData)
}
