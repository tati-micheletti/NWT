calculateDifferences <- function(effects, rasList, years, limits){
  meanDiffClim <- meanDiffV <- meanDiffF <- meanDiffVeg <- meanDiffFire <- NA
  if (length(effects) == 2){
    par(mfrow = c(1, 2))
  } else {
    if (length(effects) == 1){
      par(mfrow = c(1, 1))
    } 
  }
  if ("vegetation" %in% effects){
    diffVeg1 <-
      (rasList[["LandR.CS_SCFM"]][[paste0("year", max(years))]] -
         rasList[["LandR.CS_SCFM"]][[paste0("year", min(years))]]) -
      (rasList[["LandR_SCFM"]][[paste0("year", max(years))]] -
         rasList[["LandR_SCFM"]][[paste0("year", min(years))]])
    diffVeg2 <-
      (rasList[["LandR.CS_fS"]][[paste0("year", max(years))]] -
         rasList[["LandR.CS_fS"]][[paste0("year", min(years))]]) -
      (rasList[["LandR_fS"]][[paste0("year", max(years))]] -
         rasList[["LandR_fS"]][[paste0("year", min(years))]])
    meanDiffVeg <- mean(diffVeg1, diffVeg2)
    meanDiffV <- mean(meanDiffVeg[], na.rm = TRUE)
    rangeDiff <- max(meanDiffVeg[], 
                     na.rm = TRUE)-min(meanDiffVeg[],
                                       na.rm = TRUE)
    Limits <- limits*rangeDiff
    reclassMat <- cbind(from = c(-Inf, -Limits, Limits),
                        to = c(-Limits, Limits, Inf),
                        becomes = c(-1, 0, 1))
    meanDiffVegPlot <- reclassify(meanDiffVeg, reclassMat)
    plot(meanDiffVegPlot, col = c("darkred", "lightyellow", "darkgreen"),
         main = paste0("Climate effects of Vegetation on Biomass"),
         sub = paste0("Limits set to ", limits*100, "%"))
  }
  if ("fire" %in% effects){
    diffFire1 <-
      (rasList[["LandR.CS_fS"]][[paste0("year", max(years))]] -
         rasList[["LandR.CS_fS"]][[paste0("year", min(years))]]) -
      (rasList[["LandR.CS_SCFM"]][[paste0("year", max(years))]] -
         rasList[["LandR.CS_SCFM"]][[paste0("year", min(years))]])
    diffFire2 <-
      (rasList[["LandR_fS"]][[paste0("year", max(years))]] -
         rasList[["LandR_fS"]][[paste0("year", min(years))]]) -
      (rasList[["LandR_SCFM"]][[paste0("year", max(years))]] -
         rasList[["LandR_SCFM"]][[paste0("year", min(years))]])
      meanDiffFire <- mean(diffFire1, diffFire2)
      meanDiffF <- mean(meanDiffFire[], na.rm = TRUE)
      rangeDiff <- max(meanDiffFire[], 
                       na.rm = TRUE)-min(meanDiffFire[],
                                         na.rm = TRUE)
      Limits <- limits*rangeDiff
      reclassMat <- cbind(from = c(-Inf, -Limits, Limits),
                          to = c(-Limits, Limits, Inf),
                          becomes = c(-1, 0, 1))
      meanDiffFirePlot <- reclassify(meanDiffFire, reclassMat)
      plot(meanDiffFirePlot, col = c("darkred", "lightyellow", "darkgreen"),
           main = paste0("Climate effects of Fire on Biomass"),
           sub = paste0("Limits set to ", limits*100, "%"))
  }
  if ("extremes" %in% effects){
    diffClimate <-
      (rasList[["LandR.CS_fS"]][[paste0("year", max(years))]] -
         rasList[["LandR.CS_fS"]][[paste0("year", min(years))]]) -
      (rasList[["LandR_SCFM"]][[paste0("year", max(years))]] -
         rasList[["LandR_SCFM"]][[paste0("year", min(years))]])
    meanDiffClim <- mean(diffClimate[], na.rm = TRUE)
    rangeDiff <- max(diffClimate[], 
                     na.rm = TRUE)-min(diffClimate[],
                                       na.rm = TRUE)
    Limits <- limits*rangeDiff
    reclassMat <- cbind(from = c(-Inf, -Limits, Limits),
                        to = c(-Limits, Limits, Inf),
                        becomes = c(-1, 0, 1))
    diffClimateReclass <- reclassify(diffClimate, reclassMat)
    plot(diffClimateReclass, col = c("darkred", "lightyellow", "darkgreen"),
         main = paste0("Net climate effects on Biomass"),
         sub = paste0("Limits set to ", limits*100, "%"))
  }
  
  return(list(meanDifferenceVegetation = meanDiffV,
              meanDifferenceFire = meanDiffF,
              meanDifferenceClimate = meanDiffClim,
              vegetationDifferenceRaster = meanDiffVeg,
              fireDifferenceRaster = meanDiffFire))
}