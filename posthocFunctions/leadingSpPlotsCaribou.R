leadingSpPlotsCaribou <- function(years = c(2011, 2100),
                         pathData,
                         pathOutputs,
                         Scenarios = c("LandR_SCFM", "LandR.CS_SCFM", 
                                       "LandR_fS", "LandR.CS_fS"),
                         runs = paste0("run", 1:10),
                         leadingPercentage = 0.50001,
                         treeSpecies = c("Betu_Pap","Lari_Lar","Pice_Gla",
                                         "Pice_Mar","Pinu_Ban","Popu_Tre"),
                         treeType = NULL,
                         flammableRTM,
                         rasterToMatch,
                         useProportionLeading = FALSE){
  Require::Require("reproducible")
  outputFolder <- checkPath(file.path(pathOutputs, "vegetationPlots"), create = TRUE)
  
  #  ~~~ LEADING SPECIES ~~~~~~~~~~~~~~~~~~
  
  allScenarios <- lapply(Scenarios, function(scenario){
    tic(paste0("Calculating leading species change for ", scenario))
    allRuns <- lapply(runs, function(RUN){
      # FOR YEAR 2011
      bothYears <- lapply(years, function(Y){
        coh <- bringObjectTS(path = file.path(pathData, paste(scenario, RUN, sep = "_")),
                             rastersNamePattern = c("cohortData", Y))[[1]]
        ras <- bringObjectTS(path = file.path(pathData, paste(scenario, RUN, sep = "_")), 
                             rastersNamePattern = c("pixelGroupMap", Y))[[1]]
        cohortReduced <- coh[, list(sumBio = sum(B, na.rm = TRUE)), by = c("speciesCode", "pixelGroup")]
        biomassStack <- raster::stack(lapply(treeSpecies, function(tSp){
          message(paste0("Creating biomass map for ", tSp))
          r <- SpaDES.tools::rasterizeReduced(reduced = cohortReduced[speciesCode == tSp, ], 
                                              fullRaster = ras,
                                              newRasterCols = "sumBio", 
                                              mapcode = "pixelGroup")
          r[is.na(r[])] <- 0
          r[is.na(rasterToMatch)] <- NA
          return(r)
        }))
        names(biomassStack) <- treeSpecies
        biomassDT <- data.table(pixelID = 1:raster::ncell(biomassStack),
                                raster::getValues(biomassStack))
        biomassDT[, totalBiomass := rowSums(.SD, na.rm = TRUE), .SDcols = names(biomassDT)[names(biomassDT) != "pixelID"]]
        biomassDT <- biomassDT[totalBiomass != 0,]
        biomassDT[, leading := apply(.SD, 1, .defineLeading,
                                     leadingPercentage = leadingPercentage,
                                     totalCol = "totalBiomass"),
                  .SDcols = names(biomassDT)[names(biomassDT) != "pixelID"]]
        # Reclassify leading to conifer or deciduous
        if (is.null(treeType)){
          warning(paste0("treeType is null. Creating the following table. ",
                         "\nIf the species do not match, please provide treeType table ",
                         "with the correct classification in a column named 'newClass'"), 
                  immediate. = TRUE)
          treeType <- structure(list(ID = c(1L, 2L, 3L, 4L, 5L, 6L, 71L, 72L, 73L, 
                                            74L, 75L, 76L), landcover = c("Betu_Pap", "Lari_Lar", "Pice_Gla", 
                                                                          "Pice_Mar", "Pinu_Ban", "Popu_Tre", "Mixed_Betu_Pap", "Mixed_Lari_Lar", 
                                                                          "Mixed_Pice_Gla", "Mixed_Pice_Mar", "Mixed_Pinu_Ban", "Mixed_Popu_Tre"
                                            ), leadingType = c("deciduous", "deciduous", "conifer", "conifer", 
                                                               "conifer", "deciduous", "mixed", "mixed", "mixed", "mixed", "mixed", 
                                                               "mixed"), newClass = c(1, 1, 0, 0, 0, 1, 0.5, 0.5, 0.5, 0.5, 
                                                                                      0.5, 0.5)), row.names = c(NA, -12L), class = "data.frame")
          names(treeType)[names(treeType) == "ID"] <- "leading"
          print(treeType)
        }
        biomassDT <- merge(biomassDT, treeType[, c("leading","newClass")])
        allPixels <- data.table(pixelID = 1:raster::ncell(biomassStack))
        biomassDTfilled <- merge(allPixels, biomassDT, all.x = TRUE, by = "pixelID")
        leadingSpeciesRaster <- raster::setValues(raster(biomassStack),
                                                  biomassDTfilled[["newClass"]])
        names(leadingSpeciesRaster) <- paste("biomassMap", scenario, RUN, Y, sep = "_")
        return(leadingSpeciesRaster)
      })
      names(bothYears) <- paste0("Year", years)
      rasLastYear <- bothYears[[paste0("Year", years[length(years)])]]
      rasFirstYear <- -bothYears[[paste0("Year", years[1])]]
      leadingStackChange <- calc(stack(rasLastYear, rasFirstYear), 
                                 fun = sum,
                                 na.rm = TRUE)
      testthat::expect_true(all(minValue(leadingStackChange) >= -1 , maxValue(leadingStackChange) <= 1))
      leadingStackChange[is.na(rasterToMatch)] <- NA
      names(leadingStackChange) <- paste("leadingMapChange", scenario, RUN, sep = "_")
      return(leadingStackChange)
    })
    names(allRuns) <- runs
    return(allRuns)
  })
names(allScenarios) <- Scenarios

  # DO EACH CLIMATE SCENARIO 
eachScenarioAverage <- lapply(names(allScenarios), FUN = function(eachScenario){
  rasStk <- raster::stack(allScenarios[[eachScenario]])
  biomassDiffPath <- file.path(outputFolder, paste("averageChange",
                                                   eachScenario, "Biomass",
                                                   sep = "_"))
  
  biomassDiffPlotPath <- file.path(outputFolder, paste0(paste("averageChange",
                                                              eachScenario, "Biomass",
                                                              sep = "_"), ".png"))
  climateDiffAverage <- calc(x = rasStk, fun = mean, 
                             na.rm = TRUE,
                             filename = biomassDiffPath,
                             overwrite = TRUE,
                             format = "GTiff")
  averageChange <- 100*(mean(climateDiffAverage[], na.rm = TRUE))

  # Now plotting
  library(viridis)
  
  pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
  pal[6] <- "#f7f4f2"
  
  maxV <- max(abs(round(minValue(climateDiffAverage), 1)),
              abs(round(maxValue(climateDiffAverage), 1)))
  
  AT <- seq(-maxV, maxV, length.out = 12)
  
  climateDiffAverage[is.na(flammableRTM)] <- NA
  if (!file.exists(biomassDiffPlotPath)){
    png(filename = biomassDiffPlotPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(climateDiffAverage,
                    sub = paste0("Proportional change in leading species under GCM ", 
                                 eachScenario, 
                                 "\nRed: conversion to conifer \nBlue: conversion to deciduous"),
                    margin = FALSE,
                    maxpixels = 7e6,
                    at = AT,
                    colorkey = list(
                      space = 'bottom',
                      axis.line = list(col = 'black'),
                      width = 0.75
                    ),
                    par.settings = list(
                      strip.border = list(col = 'transparent'),
                      strip.background = list(col = 'transparent'),
                      axis.line = list(col = 'transparent')),
                    scales = list(draw = FALSE),
                    col.regions = pal,
                    par.strip.text = list(cex = 0.8,
                                          lines = 1,
                                          col = "black")))
    dev.off()
  }
  
  toc()
  return(list(ras = climateDiffAverage, averageChangePerc = averageChange))
  
})
names(eachScenarioAverage) <- names(allScenarios)

  # DO A MEAN ONE FOR ALL SCENARIOS
  rasStk <- raster::stack(unlist(allScenarios))
  biomassDiffPath <- file.path(outputFolder, paste("averageChange",
                                                   "allScenarios", "Leading",
                                                   sep = "_"))
  
  biomassDiffPlotPath <- file.path(outputFolder, paste0(paste("averageChange",
                                                              "allScenarios", "Leading",
                                                              sep = "_"), ".png"))
  climateDiffAverage <- calc(x = rasStk, fun = mean, 
                             na.rm = TRUE,
                             filename = biomassDiffPath,
                             overwrite = TRUE,
                             format = "GTiff")
  averageChange <- 100*(mean(climateDiffAverage[], na.rm = TRUE))
  
  # Now plotting
  library(viridis)
  pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
  pal[6] <- "#f7f4f2"
  
  maxV <- max(abs(round(minValue(climateDiffAverage), 1)),
              abs(round(maxValue(climateDiffAverage), 1)))
  
  AT <- seq(-maxV, maxV, length.out = 12)
  
  climateDiffAverage[is.na(flammableRTM)] <- NA
  if (!file.exists(biomassDiffPlotPath)){
    png(filename = biomassDiffPlotPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(climateDiffAverage,
                    sub = paste0("Average proportional change in leading species under all GCMs ",
                                 "\nRed: conversion to conifer \nBlue: conversion to deciduous"),
                    margin = FALSE,
                    maxpixels = 7e6,
                    at = AT,
                    colorkey = list(
                      space = 'bottom',
                      axis.line = list(col = 'black'),
                      width = 0.75
                    ),
                    par.settings = list(
                      strip.border = list(col = 'transparent'),
                      strip.background = list(col = 'transparent'),
                      axis.line = list(col = 'transparent')),
                    scales = list(draw = FALSE),
                    col.regions = pal,
                    par.strip.text = list(cex = 0.8,
                                          lines = 1,
                                          col = "black")))
    dev.off()
  }
  
  toc()

  # Make the tables better
  tb <- unlist(lapply(eachScenarioAverage, `[[`, "averageChangePerc"))
  finalTable <- data.table(climateScenario = c(names(tb), "allScenarios"),
                           averageChange = c(as.numeric(tb), averageChange))
  write.csv(finalTable, file = file.path(outputFolder, paste("averageChangeInLeadingSp.csv")))
  
return(list(eachScenario = eachScenarioAverage, 
            allScenarios = list(ras = climateDiffAverage,
                                averageChangePerc = averageChange)))
}

.defineLeading <- function(x, leadingPercentage = 0.8, totalCol){
  colID <- which(x[-length(x)] > (leadingPercentage*x[[totalCol]]))
  if (length(colID) == 0){
    # If we don't have a leading, we need to id conifer leading,
    # or deciduous leading
    colID1 <- which.max(x[-length(x)])
    colID <- as.integer(paste0(length(x), colID1))
  }
  return(colID)
}
