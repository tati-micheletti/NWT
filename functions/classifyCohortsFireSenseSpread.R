classifyCohortsFireSenseSpread <- function(cohortData, yearCohort){
  spCode <- c('Pice_Mar', 'Pice_Gla', 'Lari_Lar', 'Betu_Pap', 'Popu_Tre', 'Pinu_Ban') # TODO Make it flexible and into a function!
  reclassTable <- data.table(speciesCode = spCode, burnClass = c("class3", "class3", "class3", "class2", "class2", "class4"))
  cohortData <- merge(cohortData, reclassTable, by = "speciesCode", all.x = TRUE)
  cohortData[age < 15, burnClass := "class1"]
  cohortData[is.na(B), burnClass := "class5"]
  #Assertion
  testthat::expect_true(NROW(cohortData[is.na(totalBiomass) & burnClass != "class5", ])==0)
  testthat::expect_true(NROW(cohortData[!is.na(totalBiomass) & burnClass == "class5", ])==0)
  cohortData[, BperClass := sum(B), by = c("burnClass", "pixelGroup")]
  
  cohortData[, propBurnClassFire := BperClass/totalBiomass]
  # Fix 0/0
  cohortData[is.na(propBurnClassFire), propBurnClassFire := 0]
  testthat::expect_true(NROW(cohortData) == NROW(na.omit(cohortData)))
  
  # Remove speciesCode so I can remove duplicates (i.e. different species that make the same class)
  toRemove <- c("speciesCode", "ecoregionGroup", "rasterToMatch", "age", "B")
  cohortData[, c(toRemove) := NULL]
  cohortData <- unique(cohortData)
  
  classList <- lapply(paste0("class", 1:4), function(cls){
    cohortDataub <- cohortData[burnClass == cls, ]
    ras <- SpaDES.tools::rasterizeReduced(reduced = cohortDataub, fullRaster = pixelGroupMap2001, 
                                          newRasterCols = "propBurnClassFire", mapcode = "pixelGroup")
    names(ras) <- paste0(cls, "_", yearCohort)
    return(ras)
  })
  names(classList) <- paste0("class", 1:4)
  return(classList)
}
