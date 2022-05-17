biomassPlots <- function(years = c(2011, 2100),
                                  pathData,
                                  pathOutputs,
                                  Scenarios = c("LandR_SCFM", "LandR.CS_SCFM", 
                                                "LandR_fS", "LandR.CS_fS"),
                                  runs = paste0("run", 1:10),
                                  flammableRTM,
                         comparisons = list(vegetation = c("LandR.CS_", "LandR_"),
                                            fire = c("SCFM", "fS"),
                                            netEffect = c("LandR.CS_fS", "LandR_SCFM"))){
    Require::Require("reproducible")
    outputFolder <- checkPath(file.path(pathOutputs, "vegetationPlots"), create = TRUE)
    
      # 1. For each of below, make one map of total biomass
      # 2. Load cohort data for all runs (n=10), for all scenarios (n=4) = 40 maps
      # 3. Mix-match depending on the comparison, do the 
          # non-climate-sensitive - climate-sensitive (still in map format) per run (20 maps)
      # 4. Average these 20 maps and create one average difference map 

    allScenarios <- lapply(Scenarios, function(scenario){
      tic(paste0("Calculating biomass change for ", scenario))
        allRuns <- lapply(runs, function(RUN){
          # FOR YEAR 2011
          bothYears <- lapply(years, function(Y){
            coh <- bringObjectTS(path = file.path(pathData, scenario, RUN),
                                 rastersNamePattern = c("cohortData", Y))[[1]]
            ras <- bringObjectTS(path = file.path(pathData, scenario, RUN), 
                                 rastersNamePattern = c("pixelGroupMap", Y))[[1]]
            # FOR EACH RUN, I NEED TO EXTRACT RASTERS OF BIOMASS FOR ALL SPECIES.
            cohortReduced <- coh[, list(sumBio = sum(B, na.rm = TRUE)), by = "pixelGroup"]
            biomassStack <- SpaDES.tools::rasterizeReduced(reduced = cohortReduced, 
                                                           fullRaster = ras,
                                                           newRasterCols = "sumBio", 
                                                           mapcode = "pixelGroup")
            biomassStack[is.na(biomassStack[])] <- 0
            names(biomassStack) <- paste("biomassMap", scenario, RUN, Y, sep = "_")
            return(biomassStack)
          })
          names(bothYears) <- paste0("Year", years)

biomassStackChange <- bothYears[[paste0("Year", years[length(years)])]] -
                        bothYears[[paste0("Year", years[1])]]

names(biomassStackChange) <- paste("biomassMapChange", scenario, RUN, sep = "_")
  return(biomassStackChange)
          })
        names(allRuns) <- runs
          return(allRuns)
        })
names(allScenarios) <- Scenarios
allScenariosUnlisted <- unlist(allScenarios)
allComparisons <- names(allScenariosUnlisted)

    # Now I need to mix-match the ones that are the "replicates" (i.e. average the combinations that
    # separate each one of the effects -- fire, vegetation and netEffect)
    factorialRasters <- lapply(names(comparisons), FUN = function(eachComparison){
      tic(paste0("Biomass change calculated for ", eachComparison))
      biomassDiffPath <- file.path(outputFolder, paste("difference",
                                                            eachComparison, "Biomass",
                                                       sep = "_"))

      biomassDiffPlotPath <- file.path(outputFolder, paste0(paste("difference",
                                                           eachComparison, "Biomass",
                                                           sep = "_"), ".png"))
      
      # if (!all(file.exists(paste0(biomassDiffPath, ".tif")),
      #          file.exists(biomassDiffPlotPath))) {
        climateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][1],
                                                      x = allComparisons)])
        climateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% climateGroupNames])
        nonclimateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][2],
                                                         x = allComparisons)])
        nonclimateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% nonclimateGroupNames])
        
        climateDifference <- climateGroup - nonclimateGroup
        climateDiffAverage <- calc(x = climateDifference, fun = mean,
                                   filename = biomassDiffPath,
                                   overwrite = TRUE,
                                   format = "GTiff")
        # Now plotting
        
        pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
        pal[6] <- "#f7f4f2"
        
        typeName <- ifelse(eachComparison == "netEffect", 
                           "net climate effect", 
                           paste0("climate effect via ", eachComparison))
        
        maxV <- max(abs(round(minValue(climateDiffAverage), 1)),
                    abs(round(maxValue(climateDiffAverage), 1)))
        
        AT <- seq(-maxV, maxV, length.out = 12)
        
        nBreak <- length(pal)+1
        climateDiffAverage[is.na(flammableRTM)] <- NA
        if (!file.exists(biomassDiffPlotPath)){
          png(filename = biomassDiffPlotPath,
              width = 21, height = 29,
              units = "cm", res = 300)
          print(levelplot(climateDiffAverage,
                          sub = paste0("Difference in vegetation biomass due to ", 
                                       typeName),
                          margin = FALSE,
                          maxpixels = 6e6,
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
        return(climateDiffAverage)
      # } else {
      #   return(raster::raster(biomassDiffPath))
      # }
    })
    names(factorialRasters) <- names(comparisons)
    return(factorialRasters)
}
