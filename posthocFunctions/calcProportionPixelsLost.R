# PLOT 4 ####
calcProportionPixelsLost <- function(listOfRasters,
                                     species,
                                     comparisons,
                                     outputFolder,
                                     netChangeTable, # Needs to have the netChange column! This col is in areas 6.25*(sum(probabilities))
                                     # netChangeTable comes from plotsPaper_NotFun.R
                                     useFuture = TRUE,
                                     percentToDiscard = 0.3){
  
  outputFolder <- checkPath(file.path(outputFolder, "colonization"), create = TRUE)
  fileNamePath <- file.path(outputFolder, "proportionPixelsChangedTable.qs")
  if (!file.exists(fileNamePath)){
    if (useFuture) plan("multiprocess", workers = length(species))
    allBirds <- rbindlist(future_lapply(names(listOfRasters), function(sp){ ########### future_lapply <~~~~~~~~~~~~~~~~~~~~future_
      allScenarios <- raster::stack(lapply(names(listOfRasters[[sp]]), function(scenario){
        tic(paste0("Calculating colonization/extirpation for ", paste(sp, scenario, sep = " ")))
        allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
          allRuns <- raster::stack(lapply(names(listOfRasters[[sp]][[scenario]][[bmod]]), function(runs){
            colRasPath <- file.path(outputFolder, paste("colonization", "firstYear", sp, scenario, bmod, runs, sep = "_"))
            if (!file.exists(paste0(colRasPath, ".tif"))){
              rasStk <- listOfRasters[[sp]][[scenario]][[bmod]][[runs]]
              colRas <- calcColonization(rasT0Path = rasStk[[1]], 
                                         rasName = colRasPath)
              gc()
            } else {
              colRas <- raster::raster(paste0(colRasPath, ".tif"))
            }
            return(colRas)
          }))
          names(allRuns) <- names(listOfRasters[[sp]][[scenario]][[bmod]])
          return(allRuns)
        })
        names(allMods) <- names(listOfRasters[[sp]][[scenario]])
        toc()
        return(raster::stack(allMods))
      }))
      fileNamePath <- file.path(outputFolder, paste0("probabilityPresence_2011_", sp))
      tic(paste0("Calculating probability of presence in 2011 for ", sp))
      if (!file.exists(paste0(fileNamePath, ".tif"))){
        probPresence <- calc(x = allScenarios, fun = mean, na.rm = TRUE,
                             filename = fileNamePath,
                             format = "GTiff")
        rm(allScenarios)
        gc()
      } else {
        probPresence <- raster(paste0(fileNamePath, ".tif"))
      }
      m2ExpectedArea <- sum(probPresence[], na.rm = TRUE)
      DT <- data.table(species = sp,
                       expectedAreaHa2011 = 6.25*m2ExpectedArea,
                       expectedAreaHa2100 = sum(netChangeTable[species == sp & effect != "netEffect", netChange]),
                       proportionOfAreaChanged = (sum(netChangeTable[species == sp & effect != "netEffect", netChange]))/
                         (6.25*m2ExpectedArea))
      toc()
      return(DT)
    }))
    plan("sequential")
    qs::qsave(allBirds, fileNamePath)
  } else {
    allBirds <- qs::qread(fileNamePath)
  }
  # Now the plot
  allBirds[, species := factor(species, levels = levels(netChangeTable$species))]
  
  library("ggplot2")
  library("data.table")
  library("raster")
  library("tictoc")
  
  allBirds[, colonization := ifelse(proportionOfAreaChanged > 0, "increase", "decrease")]
  
  lapply(X = Species, function(sp){
    DT <- allBirds[species == sp,]
    signal <- ifelse(unique(DT[["proportionOfAreaChanged"]]) > 0, "> 0", "< 0")
    jit <- ifelse(unique(DT[["proportionOfAreaChanged"]]) > 0, 0.2, -0.2)
    S <-  sum(DT[eval(parse(text = paste0("proportionOfAreaChanged", 
                                          signal))), 
                 proportionOfAreaChanged])
    pos <- S + jit
    allBirds[species == sp, labelMark := pos]
    return("OK")
  })
  
  p4 <- ggplot(data = allBirds, mapping = aes(x = proportionOfAreaChanged, y = species, 
                                              fill = colonization, group = colonization,
                                              color = colonization)) +
    geom_col() +
    geom_vline(xintercept = 0, color = "black") + 
    xlab("Expected proportion of habitat area colonized or lost due to climate change") +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(values = c("increase" = "darkgreen", 
                                  "decrease" = "firebrick3")) + 
    scale_fill_manual(values = c("increase" = "forestgreen", 
                                 "decrease" = "firebrick2")) +
    geom_text(aes(x = labelMark, 
                  label = round(proportionOfAreaChanged, 2)), 
              size = 2.5, color = "grey10", check_overlap = TRUE) +
    scale_x_continuous(breaks = seq(-1, 5.3, by = 0.5))
  p4
  
  ggsave(device = "png", filename = file.path(outputFolder, "proportionalChangeInArea.png"), 
         width = 8, height = 11)
  
  return(file.path(outputFolder, "proportionalChangeInArea.png"))
  }

calcColonization <- function(rasT0Path, rasT1Path = NULL, 
                             percentToDiscard = 0.3, rasName){
  if (is(rasT0Path, "character")){
    rasT0Path <- raster::raster(rasT0Path)
  }
  if (!is.null(rasT1Path)){
    if (is(rasT1Path, "character")){
      rasT1Path <- raster::raster(rasT1Path)
    }
  }
  rasT0 <- .presenceAbsenceRas(rasT0Path, percentToDiscard)
  if (!is.null(rasT1Path)){
    rasT1 <- .presenceAbsenceRas(rasT1Path, percentToDiscard)
    rasCol <- rasT1 - rasT0
  } else {
    rasCol <- rasT0
  }
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

if (FALSE){
  factorialRasters <- lapply(names(comparisons), FUN = function(eachComparison){
    tic(paste0("Probability of colonization and extirpation calculated for ", eachComparison, 
               " for ", sp))
    colonizationDiffPath <- file.path(outputFolder, paste("difference", 
                                                          eachComparison, 
                                                          sp, "colonization", sep = "_"))
    extirpationDiffPath <- file.path(outputFolder, paste("difference", 
                                                         eachComparison, 
                                                         sp, "extirpation", sep = "_"))
    
    if (!all(file.exists(paste0(colonizationDiffPath, ".tif")),
             file.exists(paste0(extirpationDiffPath, ".tif")))){
      climateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][1], 
                                                    x = allComparisons)])
      nonclimateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][2], 
                                                       x = allComparisons)])
      
      ############### CLIMATE SENSITIVE
      climateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% climateGroupNames])
      propColonizationClimatePath <- file.path(outputFolder, paste("climateSensitive", 
                                                                   eachComparison, 
                                                                   sp, "colonization", sep = "_"))
      propExtirpationClimatePath <- file.path(outputFolder, paste("climateSensitive", 
                                                                  eachComparison, 
                                                                  sp, "extirpation", sep = "_"))
      if (!file.exists(paste0(propColonizationClimatePath, ".tif"))){
        probColClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                    filename = propColonizationClimatePath,
                                    format = "GTiff")
      } else probColClimateGroup <- raster::raster(paste0(propColonizationClimatePath, ".tif"))
      if (!file.exists(paste0(propExtirpationClimatePath, ".tif"))){
        probExtClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                    filename = propExtirpationClimatePath,
                                    format = "GTiff")
      } else probExtClimateGroup <- raster::raster(paste0(propExtirpationClimatePath, ".tif"))
      ############### NON CLIMATE SENSITIVE
      nonclimateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% nonclimateGroupNames])
      propColonizationNonClimatePath <- file.path(outputFolder, paste("nonclimateSensitive", 
                                                                      eachComparison, 
                                                                      sp, "colonization", sep = "_"))
      propExtirpationNonClimatePath <- file.path(outputFolder, paste("nonclimateSensitive", 
                                                                     eachComparison, 
                                                                     sp, "extirpation", sep = "_"))
      if (!file.exists(paste0(propColonizationNonClimatePath, ".tif"))){
        probColNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                       filename = propColonizationNonClimatePath,
                                       format = "GTiff")
      } else probColNonclimateGroup <- raster::raster(paste0(propColonizationNonClimatePath, ".tif"))
      if (!file.exists(paste0(propExtirpationNonClimatePath, ".tif"))){
        probExtNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                       filename = propExtirpationNonClimatePath,
                                       format = "GTiff")
      } else probExtNonclimateGroup <- raster::raster(paste0(propExtirpationNonClimatePath, ".tif"))
      toc()
      
      colonizationDiff <- probColClimateGroup - probColNonclimateGroup
      writeRaster(colonizationDiff, colonizationDiffPath, format = "GTiff")
      extirpationDiff <- probExtClimateGroup - probExtNonclimateGroup
      writeRaster(extirpationDiff, extirpationDiffPath, format = "GTiff")
      
      return(list(colonizationDifference = colonizationDiff,
                  extirpationDifference =  extirpationDiff))
    } else {
      return(list(colonizationDifference = raster::raster(colonizationDiffPath),
                  extirpationDifference =  raster::raster(extirpationDiffPath)))
    }
  })
  names(factorialRasters) <- names(comparisons)
  
  # And finally the one for the net climate effect on birds
  tic(paste0("Probability of colonization and extirpation calculated for the net climate effect for ", sp))
  colonizationDiffPath <- file.path(outputFolder, paste("difference", 
                                                        "netEffect", 
                                                        sp, "colonization", sep = "_"))
  extirpationDiffPath <- file.path(outputFolder, paste("difference", 
                                                       "netEffect", 
                                                       sp, "extirpation", sep = "_"))
  if (!all(file.exists(paste0(colonizationDiffPath, ".tif")),
           file.exists(paste0(extirpationDiffPath, ".tif")))){
    clim <- unlist(lapply(comparisons, `[[`, 1))
    nonclim <- unlist(lapply(comparisons, `[[`, 2))
    climateGroupNames <- sort(grepMulti(allComparisons, patterns = clim))
    nonclimateGroupNames <- sort(grepMulti(allComparisons, patterns = nonclim))
    
    ############### CLIMATE SENSITIVE
    climateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% climateGroupNames])
    propColonizationClimatePath <- file.path(outputFolder, paste("climateSensitive", 
                                                                 "netEffect", 
                                                                 sp, "colonization", sep = "_"))
    propExtirpationClimatePath <- file.path(outputFolder, paste("climateSensitive", 
                                                                "netEffect", 
                                                                sp, "extirpation", sep = "_"))
    if (!file.exists(paste0(propColonizationClimatePath, ".tif"))){
      probColClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                  filename = propColonizationClimatePath,
                                  format = "GTiff")
    } else probColClimateGroup <- raster::raster(paste0(propColonizationClimatePath, ".tif"))
    if (!file.exists(paste0(propExtirpationClimatePath, ".tif"))){
      probExtClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                  filename = propExtirpationClimatePath,
                                  format = "GTiff")
    } else probExtClimateGroup <- raster::raster(paste0(propExtirpationClimatePath, ".tif"))
    ############### NON CLIMATE SENSITIVE
    nonclimateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% nonclimateGroupNames])
    propColonizationNonClimatePath <- file.path(outputFolder, paste("nonclimateSensitive", 
                                                                    "netEffect", 
                                                                    sp, "colonization", sep = "_"))
    propExtirpationNonClimatePath <- file.path(outputFolder, paste("nonclimateSensitive", 
                                                                   "netEffect", 
                                                                   sp, "extirpation", sep = "_"))
    if (!file.exists(paste0(propColonizationNonClimatePath, ".tif"))){
      probColNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                     filename = propColonizationNonClimatePath,
                                     format = "GTiff")
    } else probColNonclimateGroup <- raster::raster(paste0(propColonizationNonClimatePath, ".tif"))
    if (!file.exists(paste0(propExtirpationNonClimatePath, ".tif"))){
      probExtNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                     filename = propExtirpationNonClimatePath,
                                     format = "GTiff")
    } else probExtNonclimateGroup <- raster::raster(paste0(propExtirpationNonClimatePath, ".tif"))
    toc()
    
    colonizationDiff <- probColClimateGroup - probColNonclimateGroup
    writeRaster(colonizationDiff, colonizationDiffPath, format = "GTiff")
    extirpationDiff <- probExtClimateGroup - probExtNonclimateGroup
    writeRaster(extirpationDiff, extirpationDiffPath, format = "GTiff")
    
    netEffect <- list(colonizationDifference = colonizationDiff,
                      extirpationDifference =  extirpationDiff)
  } else {
    netEffect <- list(colonizationDifference = raster::raster(colonizationDiffPath),
                      extirpationDifference =  raster::raster(extirpationDiffPath))        
  }
  
  factorialRasters <- c(factorialRasters, netEffect = list(netEffect))
  return(factorialRasters)
}
