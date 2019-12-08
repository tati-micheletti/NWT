
# DONE
# comparisons <- list(climate = c("climateStatic", "dynamic"),
#                     vegetation = c("LandR_", "LandR.CS_"),
#                     fire = c("fS", "SCFM"))
# source('/mnt/data/Micheletti/NWT/posthocFunctions/makeDiffAnalysis.R')
# 
# plts <- future_lapply(seq_along(comparisons), function(index){
#   pl <- makeDiffAnalysis(comparisons = comparisons[index], writeRas = TRUE)
# })

# source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
# pth <- file.path(getwd(), "outputs/23OCT19/effectsRasters/")
# birds <- c("CAWA", "OSFL", "RUBL")
# scenarios <- c("climate", "fire", "vegetation")
# plt <- makeAveragePlotTime(dataFolder = pth, 
                           # birds = c("CAWA", "OSFL", 'RUBL'), 
                           # scenarios = scenarios)


library(SpaDES)
source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
pth <- checkPath(file.path(getwd(), "outputs/06DEC19/effectsRasters/"), create = TRUE)
library(raster)
#listOfRasters <- list(cumulativeEffect_abs = list(CAWA = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run2/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedCAWAYear2100.tif"),
#                                                           raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run2/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedCAWAYear2100.tif")),
#                                              OSFL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run2/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedOSFLYear2100.tif"),
#                                                           raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run2/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedOSFLYear2100.tif")),
#                                              RUBL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run2/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedRUBLYear2100.tif"),
#                                                           raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run2/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedRUBLYear2100.tif"))))

#dRas <- makeDeltaRasters(listOfRasters = listOfRasters, 
#                        relativeDelta = FALSE,
#                        outputFolder = pth, 
#                        lightLoad = TRUE,
#                        overwrite = FALSE,
#                        upload = TRUE,
#                        folderID = list(cumulativeEffect_abs = "1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"))

listOfRasters <- list(cumulativeEffect1_abs = list(CAWA = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedCAWAYear2100.tif"),
                                                           raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run1/birdPredictionsV6dynamic/run1_LandR.CS_fSpredictedCAWAYear2100.tif")),
                                              OSFL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedOSFLYear2100.tif"),
                                                           raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run1/birdPredictionsV6dynamic/run1_LandR.CS_fSpredictedOSFLYear2100.tif")),
                                              RUBL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedRUBLYear2100.tif"),
                                                           raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run1/birdPredictionsV6dynamic/run1_LandR.CS_fSpredictedRUBLYear2100.tif"))))

dRas <- makeDeltaRasters(listOfRasters = listOfRasters, 
                         relativeDelta = FALSE,
                         outputFolder = pth, 
                         lightLoad = TRUE,
                         overwrite = FALSE,
                         upload = TRUE,
                         folderID = list(cumulativeEffect1_abs = "1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"))


rasFolder <- "/mnt/data/Micheletti/NWT/outputs/06DEC19/effectsRasters"

library(raster)
library(googledrive)
library(future)
library(future.apply)
plan("multicore")

future_lapply(c("CAWA", "OSFL", "RUBL"), function(BIRD){
  BIRD1 <- raster(file.path(rasFolder, paste0("cumulativeEffect1_abs_", BIRD, "delta.tif")))
  BIRD2 <- raster(file.path(rasFolder, paste0("cumulativeEffect_abs_", BIRD, "delta.tif")))
  BIRD1[]<-BIRD1[]
  BIRD2[]<-BIRD2[]
  averageBIRD <- calc(stack(BIRD1, BIRD2), fun = mean)
  sdBIRD <- calc(stack(BIRD1, BIRD2), fun = sd)
  aveName <- file.path(rasFolder, paste0("averageDelta_abs_", BIRD, ".tif"))
  sdName <- file.path(rasFolder, paste0("sdDelta_abs_", BIRD, ".tif"))
  writeRaster(averageBIRD, filename = aveName, format = "GTiff")
  writeRaster(sdBIRD, filename = sdName, format = "GTiff")
  lapply(c(aveName, sdName), function(ras){
    drive_upload(ras, as_id("1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"))
  })
})

