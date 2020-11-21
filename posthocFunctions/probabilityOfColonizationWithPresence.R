probabilityOfColonizationWithPresence <- function(species,
                                                  flammableRTM,
                                                  scenarioName,
                                                  folder,
                                                  patternsT0,
                                                  patternsT1,
                                                  shp = NULL) # shp is not currently functional
  {
  # Get rasters
  ras <- getT0andT1rasters(folder,
                           patternsT0,
                           patternsT1)
  
  # Make plot
  pathsToFiles <- makeProbabilityOfColonizationWithPresencePlot(rasT0 = ras[["T0"]],
                                                rasT1 = ras[["T1"]],
                                                species,
                                                folder = folder,
                                                flammableRTM,
                                                scenarioName,
                                                shp = shp)
  return(pathsToFiles)
}

getT0andT1rasters <- function(folder,
                              patternsT0,
                              patternsT1){
  ras0 <- grepMulti(list.files(folder, full.names = TRUE), patterns = c(patternsT0, ".tif"))
  ras1 <- grepMulti(list.files(folder, full.names = TRUE), patterns = c(patternsT1, ".tif"))
  
  if (any(length(ras0) == 0, length(ras1) == 0))
    stop("No .tif (raster) files found in the folder provided with the patterns provided")
  
    return(list(T0 = raster::raster(ras0), T1 = raster::raster(ras1)))
}

makeProbabilityOfColonizationWithPresencePlot <- function(
                                                  rasT1,
                                                  rasT0,
                                                  species,
                                                  flammableRTM,
                                                  scenarioName,
                                                  folder,
                                                  shp){
  message(paste0("Starting probability of colonization with presence maps for ", species))
  # MAKE LEVELPLOT
library("rasterVis")
  pal <- pals::brewer.rdylbu(100)
  speciesChangesPath <- file.path(folder, paste0("probabilityMovement2100_", scenarioName,
                                                 "_", species, ".png"))
  if (!file.exists(speciesChangesPath)){
    rasColonization <- rasT1-rasT0
    rasColonization[rasT0 == 0 & rasT1 == 0] <- NA
    rasColonization[is.na(flammableRTM)] <- NA
    m <- max(abs(cellStats(rasColonization, "max")),
             abs(cellStats(rasColonization, "min")))
    AT <- round(seq(from = -m, to = m, length.out = length(pal) + 1), 0)
    png(filename = speciesChangesPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(rasColonization,
                    sub = paste0("Probability of movement of ", species," from 2011 to 2100"),
                    margin = FALSE,
                    maxpixels = 6e6,
                    colorkey = list(
                      space = 'bottom',
                      labels = list(at = AT, font = 4),
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
                                          col = "black"))
      )
    
    dev.off()
  }
  return(speciesChangesPath)
}
