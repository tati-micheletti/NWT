biomassPlotsCaribou <- function(years = c(2011, 2100),
                                  pathData,
                                  pathOutputs,
                                  Scenarios = "LandR.CS_fS",
                                runs = paste0("run", 1:5),
                                  flammableRTM){
    Require::Require("reproducible")
    outputFolder <- checkPath(file.path(pathOutputs, "vegetationPlots"), create = TRUE)
    
    allScenarios <- lapply(Scenarios, function(scenario){
      tic(paste0("Calculating biomass change for ", scenario))
        allRuns <- lapply(runs, function(RUN){
          # FOR YEAR 2011
          bothYears <- lapply(years, function(Y){
            coh <- bringObjectTS(path = file.path(pathData, paste(scenario, RUN, sep = "_")),
                                 rastersNamePattern = c("cohortData", Y))[[1]]
            ras <- bringObjectTS(path = file.path(pathData, paste(scenario, RUN, sep = "_")), 
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
  # Now plotting
  pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
  pal[6] <- "#f7f4f2"
  library("lattice")
  library("rasterVis")
  library("viridis")
  library("maptools")
  library("colorspace")
  typeName <- paste0("Average change in tree biomass for ", eachScenario)
  
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
                    sub = typeName,
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
  return(climateDiffAverage)
 })
    names(eachScenarioAverage) <- names(allScenarios)
    
# DO A MEAN ONE FOR ALL SCENARIOS
    rasStk <- raster::stack(unlist(allScenarios))
    biomassDiffPath <- file.path(outputFolder, paste("averageChange",
                                                     "allScenarios", "Biomass",
                                                     sep = "_"))
    
    biomassDiffPlotPath <- file.path(outputFolder, paste0(paste("averageChange",
                                                                "allScenarios", "Biomass",
                                                                sep = "_"), ".png"))
    climateDiffAverage <- calc(x = rasStk, fun = mean, 
                               na.rm = TRUE,
                               filename = biomassDiffPath,
                               overwrite = TRUE,
                               format = "GTiff")
    # Now plotting
    pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
    pal[6] <- "#f7f4f2"
    library("lattice")
    library("rasterVis")
    library("viridis")
    library("maptools")
    library("colorspace")
    typeName <- paste0("Average change in tree biomass for all climate scenarios")
    
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
                      sub = typeName,
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
    
    return(list(eachScenario = eachScenarioAverage, allScenarios = climateDiffAverage))
}
