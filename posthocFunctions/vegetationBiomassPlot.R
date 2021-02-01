vegetationBiomassPlot <- function(years = c(2011, 2100),
                                  runs = paste0("run", 1:10),
                                  pathData,
                                  pathOutputs,
                                  typeSim = c("LandR_SCFM", "LandR.CS_fS"),
                                  leadingPercentage = 0.8,
                                  quickCheck = TRUE,
                                  treeSpecies = c("Betu_Pap","Lari_Lar","Pice_Gla",
                                                  "Pice_Mar","Pinu_Ban","Popu_Tre"),
                                  overwiteFinalRasters = FALSE,
                                  pal = NULL,
                                  overwritePlots = FALSE,
                                  flammableRTM) {
# Leading species is done in its own function now: leadingChangePlot
  library(rasterVis)
  #~~~~~~~~~~~~~~~~~~~~~~ Bring cohortData and pixel group #~~~~~~~~~~~~~~~~~~~~~~
  finalPlots <- lapply(typeSim, function(sim){
    cohorDataListAll <- lapply(years, function(y){
      e <- environment()
      biomassSpeciesRuns <- lapply(runs, FUN = function(RUN) {
        message(paste0("Mapping biomass for ", RUN, " for year ", 
                       y, " for scenario ", sim))
        fl <- usefulFuns::grepMulti(list.files(pathOutputs), patterns = c("averageBiomass", sim, y))
        if (any(length(fl) == 0, !isTRUE(quickCheck))){
          coh <- bringObjectTS(path = file.path(pathData, sim, RUN),
                               rastersNamePattern = c("cohortData", y))[[1]]
          ras <- bringObjectTS(path = file.path(pathData, sim, RUN), 
                               rastersNamePattern = c("pixelGroupMap", y))[[1]]
          # FOR EACH RUN, I NEED TO EXTRACT RASTERS OF BIOMASS FOR ALL SPECIES.
          # THEN I WILL AVERAGE ACROSS ALL RUNS. 
          # AND ONLY THEN I WILL DETERMINE THE LEADING SPECIES.
          cohortReduced <- coh[, list(sumBio = sum(B, na.rm = TRUE)), by = c("speciesCode", "pixelGroup")]
          biomassStack <- raster::stack(lapply(treeSpecies, function(tSp){
            r <- SpaDES.tools::rasterizeReduced(reduced = cohortReduced[speciesCode == tSp, ], 
                                                fullRaster = ras,
                                                newRasterCols = "sumBio", 
                                                mapcode = "pixelGroup")
          }))
          names(biomassStack) <- treeSpecies
          return(biomassStack)
        } else {
          message("averageBiomass rasters seem to exist, skipping loading. If this is not true, pass quickCheck = FALSE")
          return(NA)
        }
      })
      names(biomassSpeciesRuns) <- runs
      averageBiomass <- raster::stack(lapply(treeSpecies, function(tSp){
        averageBiomassFilePath <- file.path(pathOutputs, paste("averageBiomass", sim, y, tSp, sep = "_"))
        if (!file.exists(paste0(averageBiomassFilePath, ".tif"))){
          # Get from each run the specific species and make the average 
          message(paste0("Calculating average biomass across runs for ", tSp, 
                         " for year ", y, " for scenario ", sim))
          biomStk <- lapply(biomassSpeciesRuns, `[[`, tSp)
          averageSp <- calc(x = stack(biomStk), fun = mean, na.rm = TRUE,
                            filename = averageBiomassFilePath,
                            format = "GTiff")
        } else {
          # Get from each run the specific species and make the average 
          averageSp <- raster::raster(paste0(averageBiomassFilePath, ".tif"))
        }
        return(averageSp)
      }))
      names(averageBiomass) <- treeSpecies
      # Here I have the average biomass by species across all runs.
      # From this I need 2 plots:
      # 1. Total biomass: just sum the stack;
      # 2. Leading species: identify per pixel which species has more than 0.8 proportion. 
      #                     If none, make it mixed)
      #                     
      #  ~~~ TOTAL BIOMASS
      totalBiomassFilePath <- file.path(pathOutputs, paste("totalBiomass", sim, y, sep = "_"))
      if (any(!file.exists(paste0(totalBiomassFilePath, ".tif")), overwiteFinalRasters)){
        totalBiomass <- calc(x = averageBiomass, fun = sum, na.rm = TRUE,
                             filename = totalBiomassFilePath,
                             format = "GTiff")
      } else {
        totalBiomass <- raster::raster(paste0(totalBiomassFilePath, ".tif"))
      }
      return(list(biomassRaster = totalBiomass))
    })
    names(cohorDataListAll) <- paste0("year", years)
    # --> need to subtract total biomass 2100 - 2011 to present the difference!
    changedBiomass <- cohorDataListAll[[paste0("year", years[length(years)])]][["biomassRaster"]] - 
                              cohorDataListAll[[paste0("year", years[1])]][["biomassRaster"]]
    library(viridis)
    pal <- pals::brewer.rdylbu(100)
    totalBiomassPath <- file.path(pathOutputs, paste0("totalBiomass_", sim, 
                                                        ".png"))
    changedBiomass[is.na(flammableRTM)] <- NA
    if (!file.exists(totalBiomassPath)){
      png(filename = totalBiomassPath,
          width = 21, height = 29,
          units = "cm", res = 300)
      print(levelplot(changedBiomass,
                      sub = paste0("Difference in biomass from 2011 to 2100 for ", sim),
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
                      col.regions = pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                      par.strip.text = list(cex = 0.8,
                                            lines = 1,
                                            col = "black")))
      dev.off()
    }
    return(list(biomass = totalBiomassPath))
  })

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
