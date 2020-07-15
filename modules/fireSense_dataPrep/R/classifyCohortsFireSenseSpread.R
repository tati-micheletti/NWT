classifyCohortsFireSenseSpread <- function(cohortData, yearCohort, pixelGroupMap, flammable){
  spCode <- c('Pice_Mar', 'Pice_Gla', 'Lari_Lar', 
              'Betu_Pap', 'Popu_Tre', 'Pinu_Ban') 
  # TODO Make it flexible and into a function!
  reclassTable <- data.table(speciesCode = spCode, 
                             burnClass = c("class3", "class3", "class3", 
                                           "class2", "class2", "class4"))
  cohortData <- merge(cohortData, reclassTable, by = "speciesCode", all.x = TRUE)
  cohortData[age < 15, burnClass := "class1"]
  if (!"totalBiomass" %in% names(cohortData))
    cohortData[, totalBiomass := asInteger(sum(B)), by = c("pixelGroup")]
  
  #Assertion
  testthat::expect_true(NROW(cohortData[is.na(totalBiomass) & burnClass != "class5", ])==0)
  testthat::expect_true(NROW(cohortData[!is.na(totalBiomass) & burnClass == "class5", ])==0)
  cohortData[, BperClass := sum(B), by = c("burnClass", "pixelGroup")]
  
  cohortData[, propBurnClassFire := BperClass/totalBiomass]
  # Fix 0/0
  cohortData[is.na(propBurnClassFire), propBurnClassFire := 0]
  # testthat::expect_true(NROW(cohortData) == NROW(na.omit(cohortData)))
  
  # Remove speciesCode so I can remove duplicates (i.e. different species that make the same class)
  toRemove <- names(cohortData)[!names(cohortData) %in% c("pixelGroup", "burnClass", "propBurnClassFire")]
  cohortData[, c(toRemove) := NULL]
  cohortData <- unique(cohortData)
  
  classList <- lapply(paste0("class", 1:4), function(cls){
    cohortDataub <- cohortData[burnClass == cls, ]
    ras <- SpaDES.tools::rasterizeReduced(reduced = cohortDataub, fullRaster = pixelGroupMap, 
                                          newRasterCols = "propBurnClassFire", 
                                          mapcode = "pixelGroup")
    names(ras) <- paste0(cls, "_", yearCohort)
    return(ras)
  })
  
  # Identify non-forested pixels (non-ice/water/rocks) as class5
  # Pixels that are *NOT* NA in the RTM when this has been NA'ed for water, ice, and rocks, and 
  # ARE NA in the pixelGroupMap are the pixels that are class5
  class5ras <- raster(pixelGroupMap)
  pixGroupVals <- getValues(pixelGroupMap)
  flammableVals <- getValues(flammable)
  class5 <- which(is.na(pixelGroupMap[]) & !is.na(flammable[]))
  class5ras[class5] <- 1
  
  classList <- c(classList, list(class5ras))
  
  
  names(classList) <- paste0("class", 1:5)
  return(classList)
}
