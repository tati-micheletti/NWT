makeCaribouRSFAverageMap <- function(resultsFolder,
                                     runs,
                                     climateModels,
                                     outputsPath, 
                                     shp,
                                     initialYear,
                                     lastYear,
                                     binningTable, 
                                     overwriteCalc = FALSE,
                                     overwriteCalcAll = FALSE){
  
  reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
                                binningTable[["Max.Value"]],
                                binningTable[["RSF.Bin"]]), 
                          ncol = 3)
  
  # Lapply over climate models and load all reps
  booMaps <- lapply(X = climateModels, function(cm){
    runsMaps <- lapply(X = runs, function(RUN){
      fld <- file.path(resultsFolder, paste0(cm, "_", RUN))
      firstRas <- raster::raster(usefulFuns::grepMulti(list.files(fld, 
                                                                  recursive = TRUE, 
                                                                  full.names = TRUE), 
                                                       patterns = c("relativeSelectioncaribouRSF", 
                                                                    as.character(initialYear)), 
                                                       unwanted = ".png"))
      lastRas <- raster::raster(usefulFuns::grepMulti(list.files(fld, 
                                                                 recursive = TRUE, 
                                                                 full.names = TRUE), 
                                                      patterns = c("relativeSelectioncaribouRSF", 
                                                                   as.character(lastYear)), 
                                                      unwanted = ".png"))
      # Bin the ras
      # Make sure that the max value is infinite, so it accommodates any bigger value
      # than before
      reclassMatrix[nrow(reclassMatrix), 2] <- Inf
      firstRas <- raster::reclassify(x = firstRas, rcl = reclassMatrix)
      lastRas <- raster::reclassify(x = lastRas, rcl = reclassMatrix)
      message(paste0("Calculating differences between ", 
                     initialYear, " and ", lastYear, " for ", cm))
      diffRas <- lastRas-firstRas
      names(diffRas) <- paste0("DiffRas_", cm, "_", RUN)
      
      # As chatted with Eliot on April 15th, to extract the values 
      # for each polygon, we should do the averaging across replicates as
      # the last step. So I will get all points inside each heard, from all 
      # replicates and then average and get the deviation to represent the polygons's
      # mean RSF and variation 
      # 
      # Calculate the averages over the shapefile
      # Fasterize the shapefile
      caribouShapefileRas <- convertShpToRas(genericShp = shp, 
                                             rasterToMatch = diffRas,
                                             destinationPath = outputsPath,
                                             rasStk = TRUE, # Because polys overlap
                                             shapefileName = "rasterizedCaribouShp")
      
      polyRSF <- rbindlist(lapply(1:nlayers(caribouShapefileRas), function(layIndex){
        nm <- names(caribouShapefileRas)[layIndex]
        r <- caribouShapefileRas[[layIndex]]
        DT <- data.table(Area = nm,
                         climateModel = cm,
                         replicate = RUN,
                         RSF = diffRas[!is.na(r)])
        return(DT)
      }))
      return(list(diffRas = diffRas,
                  polyRSF = polyRSF))
    })
    names(runsMaps) <- runs
    allMaps <- lapply(runsMaps, `[[`, "diffRas")
    allTables <- rbindlist(lapply(runsMaps, `[[`, "polyRSF"))
    meanDiffName <- file.path(outputsPath,
                            paste0("averageRSFdiff_", cm))
    
    if (any(overwriteCalc,
            !file.exists(paste0(meanDiffName, ".tif")))){
      message(paste0("Calculating averages for ", cm))
      averageCM <- calc(stack(allMaps), fun = mean, 
                        na.rm = TRUE, overwrite = TRUE,
                        filename = meanDiffName,
                        format = "GTiff")
    } else {
      averageCM <- raster(paste0(meanDiffName, ".tif"))
    }
    
    # Values need to go from -9 to 9 (RSF is only from 1 to 10)
    sdDiffName <- file.path(outputsPath,
                            paste0("sdRSFdiff_", cm))
    if (any(overwriteCalc,
            !file.exists(paste0(sdDiffName, ".tif")))){
      message(paste0("Calculating SD for ", cm))
      sdCM <- calc(stack(allMaps), fun = sd, 
                   na.rm = TRUE, overwrite = TRUE,
                   filename = sdDiffName, 
                   format = "GTiff") 
    } else {
      sdCM <- raster(paste0(sdDiffName, ".tif"))
    }
    names(averageCM) <- "averageDifference"
    names(sdCM) <- "sdDifference"
    cmStk <- stack(averageCM, sdCM)
  
    library("lattice")
    library("rasterVis")
    library("viridis")
    library("maptools")
    library("colorspace")
    
    pathSHP <- file.path(outputsPath, "RSFshp.shp")
    if (!file.exists(pathSHP)){
      rgdal::writeOGR(obj = shp, dsn = outputsPath, "RSFshp", 
                      driver = "ESRI Shapefile")
    }
  shpLoaded <- maptools::readShapeLines(pathSHP)

  bothMaps <- rbindlist(lapply(names(cmStk), function(layName){
    
    pngPath <- file.path(outputsPath, paste0(layName, "_", cm,".png"))
    
    if (layName == "averageDifference"){
      # round these up to the integer
      cmStk[[layName]] <- round(cmStk[[layName]], 0)
      cmStk[[layName]] <- ratify(cmStk[[layName]])
      Pal <- c("#7d0125", "#a90d00", "#d22d00", "#eb5500", "#f18000", "#f6a400", "#f7c252",
               "#fbdc8a", "#fff1b2", "#ffffc9", "#cfeea8", "#a6dea1", "#7fcc9e", "#57ba9d", 
               "#32a699", "#109196", "#047c8d", "#1b6685", "#2e4f7a")
      att <- "ID"
      typePlot <- "Average"
      png(filename = pngPath,
          width = 21, height = 29,
          units = "cm", res = 300)
      print(rasterVis::levelplot(cmStk[[layName]],
                                 sub = paste0(typePlot, " RSF change for ", cm),
                                 att = att,
                                 margin = FALSE,
                                 maxpixels = 6e6,
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
                                 col.regions = Pal,
                                 par.strip.text = list(cex = 0.8,
                                                       lines = 1,
                                                       col = "black"),
                                 panel = function(...){
                                   lattice::panel.levelplot.raster(...)
                                   sp::sp.polygons(shpLoaded, fill = 'black', lwd = 2.5)
                                 }))
      
      dev.off()
    } else {
      Pal <- rev(heat.colors(n = 10+round(maxValue(cmStk[[layName]]))))
      att <- NULL
      typePlot <- "SD"
      AT <- round(minValue(cmStk[[layName]])):round(maxValue(cmStk[[layName]]))
      png(filename = pngPath,
          width = 21, height = 29,
          units = "cm", res = 300)
      print(rasterVis::levelplot(cmStk[[layName]],
                                 sub = paste0(typePlot, " RSF change for ", cm),
                                 att = att,
                                 margin = FALSE,
                                 maxpixels = 6e6,
                                 colorkey = list(
                                   space = 'bottom',
                                   at = AT,
                                   axis.line = list(col = 'black'),
                                   width = 0.75
                                 ),
                                 par.settings = list(
                                   strip.border = list(col = 'transparent'),
                                   strip.background = list(col = 'transparent'),
                                   axis.line = list(col = 'transparent')),
                                 scales = list(draw = FALSE),
                                 col.regions = Pal,
                                 par.strip.text = list(cex = 0.8,
                                                       lines = 1,
                                                       col = "black"),
                                 panel = function(...){
                                   lattice::panel.levelplot.raster(...)
                                   sp::sp.polygons(shpLoaded, fill = 'black', lwd = 2.5)
                                 }))
      
      dev.off()
    }
    DTs <- data.table(climateModel = cm,
                      fileType = typePlot, 
                      fileLocation = pngPath)
    return(DTs) # Return the location of the file 
  }))
    
   return(list(mapsFilepath = bothMaps, 
               diffRas = runsMaps, 
               meanRSFPoly = allTables))
  })
  names(booMaps) <- climateModels
  allRepsMaps <- lapply(unlist(x = lapply(booMaps, `[[`, "diffRas"),
                        recursive = FALSE), `[[`, "diffRas")

  ## Average and sd of all maps
  meanDiffName <- file.path(outputsPath,
                            paste0("averageRSFdiff"))
  if (any(overwriteCalcAll,
          !file.exists(paste0(meanDiffName, ".tif")))){
    message(paste0("Calculating average for all climate scenarios"))
    averageCM <- calc(stack(allRepsMaps), fun = mean, 
                      na.rm = TRUE, overwrite = TRUE,
                      filename = meanDiffName,
                      format = "GTiff")
  } else {
    averageCM <- raster(paste0(meanDiffName, ".tif"))
  }
  
  # Values need to go from -9 to 9 (RSF is only from 1 to 10)
  sdDiffName <- file.path(outputsPath,
                          paste0("sdRSFdiff"))
  if (any(overwriteCalcAll,
          !file.exists(paste0(sdDiffName, ".tif")))){
    message(paste0("Calculating SD for all climate scenarios"))
    sdCM <- calc(stack(allRepsMaps), fun = sd, 
                 na.rm = TRUE, overwrite = TRUE,
                 filename = sdDiffName, 
                 format = "GTiff") 
  } else {
    sdCM <- raster(paste0(sdDiffName, ".tif"))
  }
  names(averageCM) <- "averageDifference"
  names(sdCM) <- "sdDifference"
  cmStk <- stack(averageCM, sdCM)
  
  # MAKE FINAL MAPS!
  pathSHP <- file.path(outputsPath, "RSFshp.shp")
  shpLoaded <- maptools::readShapeLines(pathSHP)
  bothMapsAllCS <- rbindlist(lapply(X = names(cmStk), FUN = function(layName){
    
    pngPath <- file.path(outputsPath, paste0(layName, "_allScenarios.png"))
    
    if (layName == "averageDifference"){
      # round these up to the integer
      cmStk[[layName]] <- round(cmStk[[layName]], 0)
      cmStk[[layName]] <- ratify(cmStk[[layName]])
      Pal <- c("#7d0125", "#a90d00", "#d22d00", "#eb5500", "#f18000", "#f6a400", "#f7c252",
               "#fbdc8a", "#fff1b2", "#ffffc9", "#cfeea8", "#a6dea1", "#7fcc9e", "#57ba9d", 
               "#32a699", "#109196", "#047c8d", "#1b6685", "#2e4f7a")
      att <- "ID"
      typePlot <- "Average"
      png(filename = pngPath,
          width = 21, height = 29,
          units = "cm", res = 300)
      print(rasterVis::levelplot(cmStk[[layName]],
                                 sub = paste0(typePlot, " RSF change across all climate scenarios"),
                                 att = att,
                                 margin = FALSE,
                                 maxpixels = 6e6,
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
                                 col.regions = Pal,
                                 par.strip.text = list(cex = 0.8,
                                                       lines = 1,
                                                       col = "black"),
                                 panel = function(...){
                                   lattice::panel.levelplot.raster(...)
                                   sp::sp.polygons(shpLoaded, fill = 'black', lwd = 2.5)
                                 }))
      
      dev.off()
    } else {
      Pal <- rev(heat.colors(n = 10+round(maxValue(cmStk[[layName]]))))
      att <- NULL
      typePlot <- "SD"
      AT <- round(minValue(cmStk[[layName]])):round(maxValue(cmStk[[layName]]))
      png(filename = pngPath,
          width = 21, height = 29,
          units = "cm", res = 300)
      print(rasterVis::levelplot(cmStk[[layName]],
                                 sub = paste0(typePlot, " RSF change across all climate scenarios"),
                                 att = att,
                                 margin = FALSE,
                                 maxpixels = 6e6,
                                 colorkey = list(
                                   space = 'bottom',
                                   at = AT,
                                   axis.line = list(col = 'black'),
                                   width = 0.75
                                 ),
                                 par.settings = list(
                                   strip.border = list(col = 'transparent'),
                                   strip.background = list(col = 'transparent'),
                                   axis.line = list(col = 'transparent')),
                                 scales = list(draw = FALSE),
                                 col.regions = Pal,
                                 par.strip.text = list(cex = 0.8,
                                                       lines = 1,
                                                       col = "black"),
                                 panel = function(...){
                                   lattice::panel.levelplot.raster(...)
                                   sp::sp.polygons(shpLoaded, fill = 'black', lwd = 2.5)
                                 }))
      
      dev.off()
    }

    DTs <- data.table(climateModel = "allScenarios",
                      fileType = typePlot, 
                      fileLocation = pngPath)
    return(DTs) # Return the location of the file 
  }))

  allRepsFilepath <- rbindlist(lapply(booMaps, `[[`, "mapsFilepath")) 
  finalMapsPath <- rbindlist(list(allRepsFilepath, bothMapsAllCS))
  meanRSFPoly <- rbindlist(lapply(booMaps, `[[`, "meanRSFPoly"))
  
  qs::qsave(x = meanRSFPoly, 
            file = file.path(outputsPath, "meanRSFperPolygon.qs"))
  
  return(list(meanRSFPoly = meanRSFPoly,
              mapsFilePaths = finalMapsPath))
}
