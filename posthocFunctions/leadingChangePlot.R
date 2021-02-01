leadingChangePlot <- function(years = c(2011, 2100),
                                  runs = paste0("run", 1:10),
                                  pathData,
                                  pathOutputs,
                                  typeSim = c("LandR_SCFM", "LandR.CS_SCFM", 
                                              "LandR_fS", "LandR.CS_fS"),
                                  leadingPercentage = 0.8,
                                  quickCheck = TRUE,
                                  treeSpecies = c("Betu_Pap","Lari_Lar","Pice_Gla",
                                                  "Pice_Mar","Pinu_Ban","Popu_Tre"),
                                  overwiteFinalRasters = FALSE,
                                  pal = NULL,
                                  overwritePlots = FALSE,
                                  flammableRTM) {
  browser()

  # NEEDS REVISION!!!
  
  # Legend --> negatives are net change to conifer, positive are net change 
  # to deciduous, with increasing color intensities from 0 to 1 (and 0 to -1)
  
  # If have time, do the following too:
  
  # Climate Effect on Land Cover Change via vegetation and fire pathways
  # Calculate leading species for each pixel (40 maps)
  # Veg pathway: pair up 1 map from each of 20 maps (LandR-SCFM, LandR-FireSense) and 1 map from each of 20 maps (LandR.CS-SCFM, LandR.CS-FireSense) ... creating 20 maps of "changed or not"
  # Single map of "proportion changed via veg pathway"
  # Fire pathway: pair up 1 map from each of 20 maps (LandR-SCFM, LandR.CS-SCFM) and 1 map from each of 20 maps (LandR-FireSense, LandR.CS-FireSense) ... creating 20 maps of "changed or not"
  # Single map of "proportion changed via fire pathway"
  
  
  ############ PREVIOUS CODE #############
  allScenarios <- lapply(Scenarios, function(scenario){
    tic(paste0("Calculating biomass change for ", scenario))
    allRuns <- lapply(runs, function(RUN){
      # FOR YEAR 2011
      bothYears <- lapply(years, function(Y){
        coh <- bringObjectTS(path = file.path(pathData, scenario, RUN),
                             rastersNamePattern = c("cohortData", Y))[[1]]
        ras <- bringObjectTS(path = file.path(pathData, scenario, RUN), 
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
        
  # averageDT <- data.table(pixelID = 1:raster::ncell(biomassDT), 
  #                                 raster::getValues(biomassDT))
  # averageDT[, totalBiomass := rowSums(.SD, na.rm = TRUE), .SDcols = names(averageDT)[names(averageDT) != "pixelID"]]
  # averageDT <- averageDT[totalBiomass != 0,]
  # averageDT[, leading := apply(.SD, 1, .defineLeading, 
  #                              leadingPercentage = leadingPercentage, 
  #                              totalCol = "totalBiomass"), 
  #           .SDcols = names(averageDT)[names(averageDT) != "pixelID"]]
  # allPixels <- data.table(pixelID = 1:raster::ncell(biomassDT))
  # averageDTfilled <- merge(allPixels, averageDT, all.x = TRUE, by = "pixelID")
  # leadingSpeciesRaster <- raster::setValues(raster(biomassDT), 
  #                                           averageDTfilled$leading)
  # leadingSpeciesRaster <- ratify(leadingSpeciesRaster)
  # rat <- levels(leadingSpeciesRaster)[[1]]
  # # Remove levels if not available in the final raster!
  # if (2*length(as.character(treeSpecies)) != NROW(rat)){
  #   allexpected <- c(seq_along(treeSpecies), paste0(length(treeSpecies)+1, seq_along(treeSpecies)))
  #   toExclude <- setdiff(allexpected, as.character(rat[["ID"]]))
  #   whichToExclude <- which(allexpected %in% toExclude)
  #   lc <- c(as.character(treeSpecies), 
  #                      paste0("Mixed_", as.character(treeSpecies)))
  #   lc <- lc[-whichToExclude]
  #   rat$landcover <- lc
  # } else {
  #   rat$landcover <- c(as.character(treeSpecies), 
  #                      paste0("Mixed_", as.character(treeSpecies)))
  # }
  # levels(leadingSpeciesRaster) <- rat
  
  # library(viridis)
  # library(RColorBrewer)
  # if (any(is.null(pal), length(pal) != 2*length(treeSpecies))){
  #   if (length(pal) != 2*length(treeSpecies))
  #     message("pal does not match number of species (x2 -- for Mixed leading types). Ignoring provided parameter")
  #   pal <- brewer.pal(length(na.omit(unique(leadingSpeciesRaster[]))), "Paired")
  #   Lead <- pal[c(FALSE, TRUE)]
  #   Mixed <- pal[c(TRUE, FALSE)]
  #   pal <- c(Lead, Mixed)
  # }
  # 
  # leadingSpeciesBiomassPath <- file.path(pathOutputs, paste0("leadingSp_", sim, 
  #                                                    "_", y, ".png"))
  # if (any(!file.exists(leadingSpeciesBiomassPath), overwritePlots)){
  #   png(filename = leadingSpeciesBiomassPath,
  #       width = 21, height = 29,
  #       units = "cm", res = 300)
  #   print(levelplot(leadingSpeciesRaster,
  #                   att = "landcover",
  #                   sub = paste0("Leading species in ", y," for ", sim),
  #                   margin = FALSE,
  #                   maxpixels = 6e6,
  #                   colorkey = list(
  #                     space = 'right',
  #                     axis.line = list(col = 'black'),
  #                     width = 0.75
  #                   ),
  #                   par.settings = list(
  #                     strip.border = list(col = 'transparent'),
  #                     strip.background = list(col = 'transparent'),
  #                     axis.line = list(col = 'transparent')),
  #                   scales = list(draw = FALSE),
  #                   col.regions = pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
  #                   par.strip.text = list(cex = 0.8,
  #                                         lines = 1,
  #                                         col = "black")))
  #   dev.off()
  # }
  # 
      })
    })
  })
}