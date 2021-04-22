calcColonizationList <- function(listOfRasters, 
                                 species,
                                 years,
                                 comparisons,
                                 outputFolder,
                                 overwrite,
                                 useFuture,
                                 percentToDiscard = 0.3){
  
  outputFolder <- checkPath(file.path(outputFolder, "colonization"), create = TRUE)
  if (useFuture) plan("multiprocess", workers = length(species)/2)
  allBirds <- future_lapply(names(listOfRasters), function(sp){
    allScenarios <- lapply(names(listOfRasters[[sp]]), function(scenario){
      tic(paste0("Calculating colonization/extirpation for ", paste(sp, scenario, sep = " ")))
      allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
        allRuns <- lapply(names(listOfRasters[[sp]][[scenario]][[bmod]]), function(runs){
          colRasPath <- file.path(outputFolder, paste("colonization", sp, scenario, bmod, runs, sep = "_"))
          if (!file.exists(paste0(colRasPath, ".tif"))){
            rasStk <- listOfRasters[[sp]][[scenario]][[bmod]][[runs]]
            colRas <- calcColonization(rasT0Path = rasStk[[1]], 
                                       rasT1Path = rasStk[[nlayers(rasStk)]],
                                       rasName = colRasPath)
          }
          return(raster::raster(paste0(colRasPath, ".tif")))
        })
        names(allRuns) <- names(listOfRasters[[sp]][[scenario]][[bmod]])
        return(allRuns)
      })
      names(allMods) <- names(listOfRasters[[sp]][[scenario]])
      toc()
      return(allMods)
    })
    names(allScenarios) <- names(listOfRasters[[sp]])
    allScenariosUnlisted <- unlist(allScenarios)
    allComparisons <- names(allScenariosUnlisted)
    
    # Now I need to calculate the probability of colonization and extirpation across runs
    
    allCombs <- data.table(expand.grid(comparisons))
    allCombsNames <- paste0(as.character(allCombs[, vegetation]), 
                            as.character(allCombs[, fire]),"_" , 
                            as.character(allCombs[, climate]))
    
    # NOT SURE BELOW. Rethink if I can't really use all runs + similars as reps. This would mean
    # I need to change the way I calculates stuff above.
    # Effect Climate (40 reps): 
    #     LandR.CS_fS_V6a_run1 - LandR.CS_fS_V4_run1 = effect climate
    #     LandR_SCFM_V6a_run3 - LandR_SCFM_V4_run3 = effect climate
    # Effect Fire (40 reps): 
    #     LandR.CS_fS_V6a_run1 - LandR.CS_SCFM_V6a_run1 = effect fire
    #     LandR_fS_V4_run1 - LandR_SCFM_V4_run1 = effect fire
    # Effect Vegetation (40 reps): 
    #     LandR.CS_fS_V6a_run1 - LandR_fS_V6a_run1 = effect vegetation
    #     LandR.CS_SCFM_V4_run1 - LandR_SCFM_V4_run1 = effect vegetation
    # Net Effect (10 reps):
    #     LandR.CS_fS_V6a_run1 - LandR_SCFM_V4_run1 = net effect
    #     LandR.CS_fS_V6a_run2 - LandR_SCFM_V4_run2 = net effect
    #     
    #     Apply the .meanOfSpecificValue for both col and extirp over all the reps for each
    #     effect.
    #     
    #     IMPLEMENT  BOTH, JUST IN CASE, OR KEEP WHAT I ALREADY DID IN A SCRIPT
    #     I MIGHT NEED TO COME BACK TO IT
    
    factorialRasters <- lapply(1:NROW(allCombs), FUN = function(rowIndex){
      browser()
      modParts <- c(as.character(allCombs[rowIndex, vegetation]), 
                    as.character(allCombs[rowIndex, fire]),"_" , 
                    as.character(allCombs[rowIndex, climate]))
      modType <- paste(modType, collapse = "")
      tic(paste0("Probability of colonization and extirpation calculated for ", 
                 modType, " for ", sp))
      allRuns <- raster::stack(allComparisons[grepMulti(patterns = modParts, 
                                                        x = names(allComparisons))])
      propColonizationPath <- file.path(outputFolder, paste(modType, sp, "colonization",
                                                            sep = "_"))
      propExtirpationPath <- file.path(outputFolder, paste(modType, sp, "extirpation",
                                                           sep = "_"))
      
      if (!file.exists(paste0(propColonizationPath, ".tif"))){
        probColonization <- calc(x = allRuns, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                 filename = propColonizationPath,
                                 format = "GTiff")
      } else probColonization <- raster::raster(paste0(propColonizationPath, ".tif"))
      if (!file.exists(paste0(propExtirpationPath, ".tif"))){
        probExtirpation <- calc(x = allRuns, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                filename = propExtirpationPath,
                                format = "GTiff")
      } else probExtirpation <- raster::raster(paste0(propExtirpationPath, ".tif"))
      toc()
      return(list(colonizationProbability = probColonization,
                  extirpationProbability =  probExtirpation))
    })
    names(factorialRasters) <- allCombsNames
    
    # Now I need the difference in probability of colonization and extirpation for EACH effect
    differenceRasters <- lapply(names(comparisons), FUN = function(eachComparison){
      tic(paste0("Difference in probability of colonization and extirpation calculated for ", 
                 eachComparison, " for ", sp))
      climateGroupNames <- sort(names(factorialRasters)[grep(comparisons[[eachComparison]][1],
                                                             x = names(factorialRasters))])
      nonclimateGroupNames <- sort(names(factorialRasters)[grep(comparisons[[eachComparison]][2],
                                                                x = names(factorialRasters))])
      
      ################ COLONIZATION ################
      
      colRasPath <- file.path(outputFolder, 
                              paste(sp, eachComparison, "mean","Colonization", 
                                    sep = "_"))
      if (!file.exists(paste0(colRasPath, ".tif"))){
        colonizationClimateRas <- lapply(climateGroupNames, function(each){
          return(factorialRasters[[each]][["colonizationProbability"]])
        })
        names(colonizationRas) <- climateGroupNames
        
        colonizationNonClimateRas <- lapply(nonclimateGroupNames, function(each){
          return(factorialRasters[[each]][["colonizationProbability"]])
        })
        names(colonizationNonClimateRas) <- nonclimateGroupNames
        tic(paste0("Difference colonization rasters for ", eachComparison, " for ", sp))
        diffCol  <- stack(mapply(function(clim, nonclim){ clim - nonclim },
                                 colonizationClimateRas,  # names from first
                                 colonizationNonClimateRas))
        diffColAve <- calc(x = diffCol, fun = mean, na.rm = TRUE, 
                           filename = colRasPath,
                           format = "GTiff")
        # diffSD <- calc(x = difference, fun = sd, na.rm = TRUE, 
        #                 filename = file.path("/home/tmichele/projects/NWT/outputs/SIMULATIONS/", 
        #                                      paste(sp, eachComparison, "sd","Colonization", 
        #                                            sep = "_")),
        #                 format = "GTiff")
        
        toc()
      } else diffColAve <- raster::raster(paste0(colRasPath, ".tif"))
      
      
      ################ EXTIRPATION #################
      
      extRasPath <- file.path(outputFolder, 
                              paste(sp, eachComparison, "mean","Extirpation", 
                                    sep = "_"))
      if (!file.exists(paste0(extRasPath, ".tif"))){
        extirpationClimateRas <- lapply(climateGroupNames, function(each){
          return(factorialRasters[[each]][["extirpationProbability"]])
        })
        names(extirpationRas) <- climateGroupNames
        
        extirpationNonClimateRas <- lapply(nonclimateGroupNames, function(each){
          return(factorialRasters[[each]][["extirpationProbability"]])
        })
        names(extirpationNonClimateRas) <- nonclimateGroupNames
        tic(paste0("Difference extirpation rasters for ", eachComparison, " for ", sp))
        diffExt  <- stack(mapply(function(clim, nonclim){ clim - nonclim },
                                 extirpationClimateRas,  # names from first
                                 extirpationNonClimateRas))
        diffExtAve <- calc(x = diffExt, fun = mean, na.rm = TRUE, 
                           filename = extRasPath,
                           format = "GTiff")
        toc()
      } else diffExtAve <- raster::raster(paste0(extRasPath, ".tif"))
      
      return(list(colonizationRaster = diffColAve,
                  extirpationRaster = diffExtAve))
    })
    names(differenceRasters) <- names(comparisons)
    
    # Now I need the difference in probability of colonization and extirpation for NET effect
    differenceRasters2 <- lapply("netEffect", FUN = function(eachComparison){
      tic(paste0("Difference in probability of colonization and extirpation calculated for ", 
                 "net effect", " for ", sp))
      # Assumption that the climate sensitive versions are always the first ones
      climateGroupNames <- allCombsNames[1] 
      nonclimateGroupNames <- allCombsNames[length(allCombsNames)]
      
      ################ COLONIZATION ################
      
      browser() # FINISH BELOW AS IS, SAVE, THEN IMPLEMENT THE OTHER APPROACH
      colRasPath <- file.path(outputFolder, 
                              paste(sp, "netEffect", "mean","Colonization", 
                                    sep = "_"))
      if (!file.exists(paste0(colRasPath, ".tif"))){
        colonizationClimateRas <- factorialRasters[[climateGroupNames]][["colonizationProbability"]]
        colonizationNonClimateRas <- factorialRasters[[nonclimateGroupNames]][["colonizationProbability"]]
        
        tic(paste0("Difference colonization rasters for net effect for ", sp))
        diffCol  <- colonizationClimateRas - colonizationNonClimateRas
        diffColAve <- calc(x = diffCol, fun = mean, na.rm = TRUE, 
                           filename = colRasPath,
                           format = "GTiff")
        toc()
      } else diffColAve <- raster::raster(paste0(colRasPath, ".tif"))
      
      
      ################ EXTIRPATION #################
      # 
      # extRasPath <- file.path(outputFolder, 
      #                         paste(sp, eachComparison, "mean","Extirpation", 
      #                               sep = "_"))
      # if (!file.exists(paste0(extRasPath, ".tif"))){
      #   extirpationClimateRas <- lapply(climateGroupNames, function(each){
      #     return(factorialRasters[[each]][["extirpationProbability"]])
      #   })
      #   names(extirpationRas) <- climateGroupNames
      #   
      #   extirpationNonClimateRas <- lapply(nonclimateGroupNames, function(each){
      #     return(factorialRasters[[each]][["extirpationProbability"]])
      #   })
      #   names(extirpationNonClimateRas) <- nonclimateGroupNames
      #   tic(paste0("Difference extirpation rasters for ", eachComparison, " for ", sp))
      #   diffExt  <- stack(mapply(function(clim, nonclim){ clim - nonclim },
      #                            extirpationClimateRas,  # names from first
      #                            extirpationNonClimateRas))
      #   diffExtAve <- calc(x = diffExt, fun = mean, na.rm = TRUE, 
      #                      filename = extRasPath,
      #                      format = "GTiff")
      #   toc()
      # } else diffExtAve <- raster::raster(paste0(extRasPath, ".tif"))
      # 
      # return(list(colonizationRaster = diffColAve,
      #             extirpationRaster = diffExtAve))
    })
    names(differenceRasters2) <- "netEffect"
    
    differenceRasters <- c(differenceRasters, differenceRasters2) 
    
    browser()
    
    return()
  })
  plan("sequential")
  names(allBirds) <- names(listOfRasters)
  return(allBirds)
}

calcColonization <- function(rasT0Path, rasT1Path, 
                             percentToDiscard = 0.3, rasName){
  if (is(rasT0Path, "character")){
    rasT0Path <- raster::raster(rasT0Path)
  }
  if (is(rasT1Path, "character")){
    rasT1Path <- raster::raster(rasT1Path)
  }
  rasT0 <- .presenceAbsenceRas(rasT0Path, percentToDiscard)
  rasT1 <- .presenceAbsenceRas(rasT1Path, percentToDiscard)
  rasCol <- rasT1 - rasT0
  writeRaster(rasCol, filename = rasName, format = "GTiff")
  return(raster::raster(paste0(rasName, ".tif")))
}

.presenceAbsenceRas <- function(ras, percentToDiscard = 0.3){
  CSdt <- data.table::data.table(pixelID = 1:ncell(ras),
                                 val = getValues(ras))
  CSdt <- na.omit(CSdt)
  data.table::setkey(CSdt, val)
  CSdt[, CUM := cumsum(val)]
  CSdt[, CUMstd := CUM/sum(val)]
  CSdt[, PA := CUMstd > percentToDiscard]
  BIRDpres <- raster(ras)
  BIRDpres[CSdt[PA == TRUE, pixelID]] <- 1
  BIRDpres[CSdt[PA == FALSE, pixelID]] <- 0
  return(BIRDpres)
}

.meanOfSpecificValue <- function(x, val) {
  return(sum(x == val)/length(x))
}
