# Revisiting plots from Birds MS

# First
# source("1_generalSetup.R")
# source("2_generatingInputs.R")

library("Require")
Require("data.table")
Require("reproducible")
Require("raster")
Require("usefulFuns")
Require("quickPlot")
Require("ggplot2")
Require("gridExtra")
Require("rasterVis")

pathOut <- checkPath("~/projects/NWT/outputs/factorialExperiment", create = TRUE)
climTable <- data.table(climateScenario = c("CS", "nonCS"),
                        landscapeScenario = c("LandR.CS_fS", "LandR_SCFM"),
                        birdScenario = c("V6a", "V4"))
# Order: BIRD
species <- c("ATSP", "REVI", "PISI", "EAPH")
species <- "ATSP"
allRuns <- paste0("run", 1:10)

studyAreaD <- aggregate(studyArea)
dev.off()
makeFigures1 <- FALSE
makeFigures2 <- TRUE
allPlots <- lapply(species, function(BIRD){

  allScenarios <- rbindlist(lapply(1:NROW(climTable), function(index){
    Scenario <- climTable[index, climateScenario]
    landscapeScenario <- climTable[index, landscapeScenario]
    birdScenario <- climTable[index, birdScenario]

    predictionsFolder <- "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/SIMULATIONS"
    # "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/SIMULATIONS/LandR.CS_fS/run1/birdPredictionsV4/run1_LandR.CS_fSpredictedALFLYear2011.tif"

  message(crayon::blue(paste0("Making predictions for ", crayon::yellow(BIRD), " for ", crayon::yellow(Scenario))))
    bird2011a <- raster::raster(file.path(predictionsFolder, landscapeScenario, "run1", 
                                         paste0("birdPredictions", birdScenario),
                                         paste0("run1", "_", landscapeScenario, "predicted",
                                                BIRD, "Year", 2011, ".tif")))
    bird2100a <- raster::raster(file.path(predictionsFolder, landscapeScenario, "run1", 
                                         paste0("birdPredictions", birdScenario),
                                         paste0("run1", "_", landscapeScenario, "predicted",
                                                BIRD, "Year", 2100, ".tif")))
    bird2011b <- raster::raster(file.path(predictionsFolder, landscapeScenario, "run10", 
                                          paste0("birdPredictions", birdScenario),
                                          paste0("run10", "_", landscapeScenario, "predicted",
                                                 BIRD, "Year", 2011, ".tif")))
    bird2100b <- raster::raster(file.path(predictionsFolder, landscapeScenario, "run10", 
                                          paste0("birdPredictions", birdScenario),
                                          paste0("run10", "_", landscapeScenario, "predicted",
                                                 BIRD, "Year", 2100, ".tif")))
    if (makeFigures1){
      pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(10)
      pal <- pal[-1]
      pal <- c(pal, "#110F24")
      
      predName <- file.path(pathOut, paste0(BIRD, "_", Scenario, "_2011_predictions.png"))
      png(filename = predName, 
          res = 300, width = 20, height = 20, units = "cm", pointsize = 11)
      plot(bird2011a, main = paste0(BIRD, " ", 2011, " ", Scenario, " run1"), col = pal, 
           frame.plot = FALSE, axes = FALSE, box=FALSE)
      dev.off()
      predName <- file.path(pathOut, paste0(BIRD, "_", Scenario, "_2100_predictions.png"))
      png(filename = predName, 
          res = 300, width = 20, height = 20, units = "cm", pointsize = 11)
      plot(bird2100a, main = paste0(BIRD, " ", 2100, " ", Scenario, " run1"), col = pal, 
           frame.plot = FALSE, axes = FALSE, box=FALSE)
      dev.off()
    }
    
    # 2. Make a histogram of the predictions for each scenario (CS or Non-CS) in 2011 and in 2100
    message(crayon::cyan(paste0("Calculating histograms for ", crayon::yellow(BIRD), " for ", crayon::yellow(Scenario))))
    v2011 <- sample(na.omit(getValues(bird2011a)), size = 1000000, replace = FALSE)
    v2100 <- sample(na.omit(getValues(bird2100a)), size = 1000000, replace = FALSE)
    DT <- data.table(Year = as.character(rep(c(2011, 2100), each = length(v2011))),
                     vals = c(v2011, v2100),
                     scenario = Scenario,
                     species = BIRD)

    # Get all deltas and make average
    message(crayon::magenta(paste0("Making delta rasters and map for ", crayon::yellow(BIRD), 
                                   " for ", crayon::yellow(Scenario))))
    deltaFilename <- file.path(pathOut,
                              paste0(BIRD, "_", Scenario, 
                                     "_averageDelta.tif"))
    if (!file.exists(deltaFilename)){
      deltasFolder <-"/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc"
      deltaMap <- stack(lapply(grepMulti(x = list.files(path = deltasFolder,
                                                        pattern = BIRD, full.names = TRUE),
                                         patterns = c(landscapeScenario, birdScenario, "delta.tif")), 
                               raster::raster))
      meanDeltaMap <- calc(x = deltaMap, fun = mean, 
                           filename = deltaFilename, 
                           format = "GTiff", overwrite = TRUE)
    } else {
      meanDeltaMap <- raster::raster(deltaFilename)
    }
    deltaMapPNG <- file.path(pathOut, paste0(BIRD, "_", Scenario, "_averageDelta.png"))
    if (!file.exists(deltaMapPNG)){
      nb.cols <- 100
      pal <- colorRampPalette(RColorBrewer::brewer.pal(9, name = "RdYlGn"))(nb.cols)
      
      png(filename = deltaMapPNG,
          width = 21, height = 29,
          units = "cm", res = 300)
      print(levelplot(meanDeltaMap,
                      sub = paste0(BIRD, " ", Scenario, " average change"),
                      margin = FALSE,
                      maxpixels = 6e6,
                      colorkey = list(
                        space = 'bottom',
                        # labels = list(at = round(seq(from = cellStats(meanDeltaMap, "min"),
                        #                              to = cellStats(meanDeltaMap, "max"),
                        #                              length.out = 11),0), font = 4),
                        axis.line = list(col = 'black'),
                        width = 0.75
                      ),
                      par.settings = list(
                        strip.border = list(col = 'transparent'),
                        strip.background = list(col = 'transparent'),
                        axis.line = list(col = 'transparent')),
                      scales = list(draw = FALSE),
                      col.regions = pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                      par.strip.text = list(cex = 0.8,
                                            lines = 1,
                                            col = "black")))
      dev.off()

    }

    return(DT)
    
  }))
  message(crayon::green(paste0("All scenarios complete for ", crayon::yellow(BIRD))))
    # 4. Make the difference maps in colonization/extirpation
  colonizationsFolder <- "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/colonization_differentThresholds/"
  message(crayon::yellow(paste0("Making colonization/extirpation rasters for ", crayon::cyan(BIRD))))

    extirpationCS <- raster::raster(grepMulti(x = list.files(path = colonizationsFolder, 
                                                             pattern = BIRD, full.names = TRUE),
                                              patterns = c("climateSensitive", "netEffect", 
                                                           "_extirpation", "tif"), 
                                              unwanted = "nonclimate"))
    colonizationCS <- raster::raster(grepMulti(x = list.files(path = colonizationsFolder, 
                                                              pattern = BIRD, full.names = TRUE),
                                               patterns = c("climateSensitive", "netEffect", 
                                                            "_colonization", "tif"), 
                                               unwanted = "nonclimate"))
    extirpationNonCS <- raster::raster(grepMulti(x = list.files(path = colonizationsFolder, 
                                                                pattern = BIRD, full.names = TRUE),
                                                 patterns = c("nonclimateSensitive", "netEffect", 
                                                              "_extirpation", "tif")))
    colonizationNonCS <- raster::raster(grepMulti(x = list.files(path = colonizationsFolder, 
                                                                 pattern = BIRD, full.names = TRUE),
                                                  patterns = c("nonclimateSensitive", "netEffect", 
                                                               "_colonization", "tif")))
    if (makeFigures2){
      browser()
    pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(10)
    pal <- pal[-1]
    pal <- c(pal, "#110F24")
    jpeg(filename = file.path(pathOut, paste0(BIRD, "_CS_E.jpg")), 
         res = 300, width = 20, height = 20, units = "cm", pointsize = 11)
    plot(extirpationCS, main = paste0("Climate Sensitive Extirpation ", BIRD), col = pal, 
         frame.plot = FALSE, axes = FALSE, box=FALSE)
    dev.off()
    
    jpeg(filename = file.path(pathOut, paste0(BIRD, "_CS_C.jpg")), 
         res = 300,width = 20, height = 20, units = "cm", pointsize = 11)
    plot(colonizationCS, main = paste0("Climate Sensitive Colonization ", BIRD), col = pal, 
         frame.plot = FALSE, axes = FALSE, box=FALSE)
    dev.off()
    
    jpeg(filename = file.path(pathOut, paste0(BIRD, "_nCS_E.jpg")), 
         res = 300, width = 20, height = 20, units = "cm", pointsize = 11)
    plot(extirpationNonCS, main = paste0("Non Climate Sensitive Extirpation ", BIRD), col = pal, 
         frame.plot = FALSE, axes = FALSE, box=FALSE)
    dev.off()
    
    jpeg(filename = file.path(pathOut, paste0(BIRD, "_nCS_C.jpg")), 
         res = 300, width = 20, height = 20, units = "cm", pointsize = 11)
    plot(colonizationNonCS, main = paste0("Non Climate Sensitive Colonization ", BIRD), col = pal, 
         frame.plot = FALSE, axes = FALSE, box=FALSE)
    dev.off()
    
    # Differences
    nb.cols <- 100
    pal <- colorRampPalette(RColorBrewer::brewer.pal(9, name = "RdYlGn"))(nb.cols)
    
    jpeg(filename = file.path(pathOut, paste0(BIRD, "_CS_diff.jpg")), 
         res = 300, width = 20, height = 20, units = "cm", pointsize = 11)
    plot(colonizationCS - extirpationCS, main = paste0("Climate sensitive net probability \nof change in occupancy ", BIRD), col = pal, 
         frame.plot = FALSE, axes = FALSE, box=FALSE)
    dev.off()
    
    jpeg(filename = file.path(pathOut, paste0(BIRD, "_nCS_diff.jpg")), 
         res = 300,width = 20, height = 20, units = "cm", pointsize = 11)
    plot(colonizationNonCS - extirpationNonCS, main = paste0("Non climate sensitive net probability \nof change in occupancy ", BIRD), col = pal, 
         frame.plot = FALSE, axes = FALSE, box=FALSE)
    dev.off()
    
    
  }
  return(allScenarios)
})

message(crayon::cyan(paste0("Making histograms for ", crayon::green(BIRD))))

allPlots2 <- rbindlist(allPlots)

P1 <- ggplot(allPlots2, aes(x = vals, fill = Year))+
  geom_histogram(position = position_dodge2(), bins = 20) +
  facet_grid(Year ~ scenario) +
  xlim(c(0, 0.1))
P1
ggsave(filename = file.path(pathOut, "histograms.png"), plot = P1, device = "png", 
       width = 21, height = 29, units = "cm", dpi = 300)
P
