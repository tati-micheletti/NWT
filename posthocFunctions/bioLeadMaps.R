bioLeadMaps <- function(Year = 2011,
                        run = "run1",
                        climateScenario,
                        pathData,
                        pal = NULL,
                        pathOutputs,
                        shp = NULL,
                        leadingPercentage = 0.75,
                        treeSpecies = c("Betu_Pap",
                                        "Lari_Lar",
                                        "Pice_Gla",
                                        "Pice_Mar",
                                        "Pinu_Ban",
                                        "Popu_Tre")){
  Require("rasterVis")
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
  
  
  coh <- bringObjectTS(path = file.path(pathData, paste0(climateScenario, "_", run)),
                       rastersNamePattern = c("cohortData", Year))[[1]]
  ras <- bringObjectTS(path = file.path(pathData, paste0(climateScenario, "_", run)),
                       rastersNamePattern = c("pixelGroupMap", Year))[[1]]
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
  
  #  ~~~ LEADING SPECIES
  biomassDT <- data.table(pixelID = 1:raster::ncell(biomassStack), 
                          raster::getValues(biomassStack))
  biomassDT[, totalBiomass := rowSums(.SD, na.rm = TRUE), .SDcols = names(biomassDT)[names(biomassDT) != "pixelID"]]
  biomassDT <- biomassDT[totalBiomass != 0,]
  biomassDT[, leading := apply(.SD, 1, .defineLeading, 
                               leadingPercentage = leadingPercentage, 
                               totalCol = "totalBiomass"), 
            .SDcols = names(biomassDT)[names(biomassDT) != "pixelID"]]
  allPixels <- data.table(pixelID = 1:raster::ncell(biomassStack))
  biomassDTfilled <- merge(allPixels, biomassDT, all.x = TRUE, by = "pixelID")
  leadingSpeciesRaster <- raster::setValues(raster(biomassStack[[1]]), 
                                            biomassDTfilled$leading)
  leadingSpeciesRaster <- ratify(leadingSpeciesRaster)
  rat <- levels(leadingSpeciesRaster)[[1]]
  # Remove levels if not available in the final raster!
  if (2*length(as.character(treeSpecies)) != NROW(rat)){
    allexpected <- c(seq_along(treeSpecies), paste0(length(treeSpecies)+1, seq_along(treeSpecies)))
    toExclude <- setdiff(allexpected, as.character(rat[["ID"]]))
    whichToExclude <- which(allexpected %in% toExclude)
    lc <- c(as.character(treeSpecies), 
            paste0("Mixed_", as.character(treeSpecies)))
    lc <- lc[-whichToExclude]
    rat$landcover <- lc
  } else {
    rat$landcover <- c(as.character(treeSpecies), 
                       paste0("Mixed_", as.character(treeSpecies)))
  }
  levels(leadingSpeciesRaster) <- rat
  raster::writeRaster(leadingSpeciesRaster, file.path(pathOutputs, paste0("leadingSpeciesRaster_",
                                                                          paste0(climateScenario, 
                                                                                 "_", run), "_", 
                                                                          Year, ".tif")), 
                      overwrite=TRUE)
  
  library(viridis)
  library(RColorBrewer)
  if (any(is.null(pal), length(pal) != 2*length(treeSpecies))){
    if (length(pal) != 2*length(treeSpecies))
      message("pal does not match number of species (x2 -- for Mixed leading types). Ignoring provided parameter")
    pal <- brewer.pal(length(na.omit(unique(leadingSpeciesRaster[]))), "Paired")
    Lead <- pal[c(FALSE, TRUE)]
    Mixed <- pal[c(TRUE, FALSE)]
    pal <- c(Lead, Mixed)
  }
  
  leadingSpeciesBiomassPath <- file.path(pathOutputs, paste0("leadingSp_",
                                                             paste0(climateScenario, "_", run),
                                                             "_", Year, ".png"))
  png(filename = leadingSpeciesBiomassPath,
      width = 21, height = 29,
      units = "cm", res = 300)
  print(levelplot(leadingSpeciesRaster,
                  att = "landcover",
                  sub = paste0("Leading species in ", Year), 
                               # " for ", climateScenario),
                  margin = FALSE,
                  maxpixels = 6e6,
                  colorkey = list(
                    space = 'right',
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
                                        col = "black"),
                  panel = function(...){
                    lattice::panel.levelplot.raster(...)
                    sp::sp.polygons(shp, fill = 'black', lwd = 1)
                  }))
  dev.off()
  
  return(leadingSpeciesBiomassPath)
  
}