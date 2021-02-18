makeCaribouRSFAverageMap <- function(resultsFolder,
                                     runs,
                                     climateModels,
                                     outputsPath, 
                                     shp,
                                     initialYear,
                                     lastYear,
                                     binningTable, 
                                     overwriteCalc = FALSE){
  
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
      return(diffRas)
    })
    meanDiffName <- file.path(outputsPath,
                            paste0("averageRSFdiff_", cm))
    
    if (any(overwriteCalc,
            !file.exists(paste0(meanDiffName, ".tif")))){
      message(paste0("Calculating averages for ", cm))
      averageCM <- calc(stack(runsMaps), fun = mean, 
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
      sdCM <- calc(stack(runsMaps), fun = sd, 
                   na.rm = TRUE, overwrite = TRUE,
                   filename = sdDiffName, 
                   format = "GTiff") 
    } else {
      sdCM <- raster(paste0(sdDiffName, ".tif"))
    }
    names(averageCM) <- "averageDifference"
    names(sdCM) <- "sdDifference"
    cmStk <- stack(averageCM, sdCM)
    # Calculate the averages over the shapefile
    # Fasterize the shapefile
    caribouShapefileRas <- convertShpToRas(genericShp = shp, 
                                           rasterToMatch = averageCM,
                                           destinationPath = outputsPath,
                                           rasStk = TRUE, # Because polys overlap
                                           shapefileName = "rasterizedCaribouShp")

    calcMeanRSF <- rbindlist(lapply(1:nlayers(caribouShapefileRas), function(layIndex){
      nm <- names(caribouShapefileRas)[layIndex]
      r <- caribouShapefileRas[[layIndex]]
      message("Averaging RSF difference values for ", nm)
      aveRSF <- mean(averageCM[!is.na(r)], na.rm = TRUE)
      # sdRSF <- mean(sdCM[!is.na(r)], na.rm = TRUE)
      DT <- data.table(Area = nm,
                       climateModel = cm,
                       averageRSF = aveRSF)
    }))
    
    # Make maps for these averages
    pal <- c("#c2473d",
             "#d96b52",
             "#e1936b",
             "#f4bd81",
             "#f8e8b0",
             "#ecedc2",
             "#bec3c7",
             "#95a3ca",
             "#6c7ed0",
             "#3260c6") # Handmade to match DeMars 2019
    
    library("lattice")
    library("rasterVis")
    library("viridis")
    library("maptools")
    
    pathSHP <- file.path(outputsPath, "RSFshp.shp")
    if (!file.exists(pathSHP)){
      rgdal::writeOGR(obj = shp, dsn = outputsPath, "RSFshp", 
                      driver = "ESRI Shapefile")
    }
  shpLoaded <- maptools::readShapeLines(pathSHP)

    # Add shp to levelplot
  bothMaps <- rbindlist(lapply(names(cmStk), function(layName){
    
    pngPath <- file.path(outputsPath, paste0(layName, "_", cm,".png"))
    
    if (layName == "averageDifference"){
      cmStk[[layName]] <- ratify(cmStk[[layName]])
      Pal <- pal
      typePlot <- "Average"
    } else {
      Pal <- viridis_pal(option = "D")(10) 
      att <- NULL
      typePlot <- "SD"
    }
    png(filename = pngPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    browser()
    print(rasterVis::levelplot(cmStk[[layName]],
                               sub = paste0(typePlot, " RSF for ", cm),
                               att = att,
                               margin = FALSE,
                               maxpixels = 6e6,
                               colorkey = list(
                                 space = 'bottom',
                                 # at = -9:9,
                                 axis.line = list(col = 'black'),
                                 width = 0.75
                               ),
                               par.settings = list(
                                 strip.border = list(col = 'transparent'),
                                 strip.background = list(col = 'transparent'),
                                 axis.line = list(col = 'transparent')),
                               scales = list(draw = FALSE),
                               col.regions = Pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                               par.strip.text = list(cex = 0.8,
                                                     lines = 1,
                                                     col = "black"),
                               panel = function(...){
                                 lattice::panel.levelplot.raster(...)
                                 sp::sp.polygons(shpLoaded, fill = 'black', lwd = 1)
                               }))
    
    dev.off()
    DTs <- data.table(climateModel = cm,
                      fileType = typePlot, 
                      fileLocation = pngPath)
    return(DTs) # Return the location of the file 
  }))
    
   return(list(mapsFilepath = bothMaps, 
               diffRas = runsMaps, 
               meanRSFPoly = calcMeanRSF))

  })
  
  # booMaps$mapsFilepath # To return
  
  diffRas <- unlist(booMaps$diffRas, recursive = FALSE) # Probably unlist one level
  
  meanRSFPoly <- rbindlist(booMaps$meanRSFPoly) # Table with the mean by poly
  browser()
  # Average those and make SD. Save (Appendix)
  # Calculate the diff per polygon
  # Make maps, save png
}
