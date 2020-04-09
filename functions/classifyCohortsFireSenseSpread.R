classifyCohortsFireSenseSpread <- function(cohortData, yearCohort, pixelGroupMap, flammable){
  spCode <- c('Pice_Mar', 'Pice_Gla', 'Lari_Lar', 'Betu_Pap', 'Popu_Tre', 'Pinu_Ban') # TODO Make it flexible and into a function!
  reclassTable <- data.table(speciesCode = spCode, burnClass = c("class3", "class3", "class3", "class2", "class2", "class4"))
  cohortData <- merge(cohortData, reclassTable, by = "speciesCode", all.x = TRUE)
  cohortData[age < 15, burnClass := "class1"]
  # cohortData[is.na(B), burnClass := "class5"] # Potentially not happening here.... We should not have is.na(B)
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
    ras <- SpaDES.tools::rasterizeReduced(reduced = cohortDataub, fullRaster = pixelGroupMap, 
                                          newRasterCols = "propBurnClassFire", 
                                          mapcode = "pixelGroup")
    names(ras) <- paste0(cls, "_", yearCohort)
    return(ras)
  })
  
  ####################### Prep Layers: Identify non-forested pixels (non-ice/water/rocks) as class5
  # Pixels that are *NOT* NA in the RTM when this has been NA'ed for water, ice, and rocks, and 
  # ARE NA in the pixelGroupMap are the pixels that are class5
browser()
  class5ras <- raster(pixelGroupMap)
  class5 <- pixelGroupMap[is.na(pixelGroupMap) & !is.na(flammable)]
  class5ras[class5pix] <- 1

  classList <- c(classList, list(class5ras))
  # # To create the class5, I need to do 1-sum(class1:4)
  # class5_2001 <- calc(x = stack(classList2001), fun = sum, na.rm = TRUE)
  # classList2001[["class5"]] <- 1 - class5_2001
  # names(classList2001[["class5"]]) <- "class5_2001"
  # class5_2011 <- calc(x = stack(classList2011), fun = sum, na.rm = TRUE)
  # classList2011[["class5"]] <- 1 - class5_2011
  # names(classList2011[["class5"]]) <- "class5_2011"
  #
  # # CHECK CLASS5 is only 1 or 0
  # #TODO

  names(classList) <- paste0("class", 1:5)
  return(classList)
}
