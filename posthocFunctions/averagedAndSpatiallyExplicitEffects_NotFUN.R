library(SpaDES)
library(reproducible)
library(data.table)
library(usefun)
library(future)
library(future.apply)
comparisons <- list(climate = c("V4dynamic", "V6dynamic"),
                    vegetation = c("LandR_", "LandR.CS_"),
                    fire = c("fS", "SCFM"))
source('/mnt/data/Micheletti/NWT/posthocFunctions/makeDiffAnalysis.R')
# 
plts <- future_lapply(seq_along(comparisons), function(index){
  pl <- makeDiffAnalysis(comparisons = comparisons[index], writeRas = TRUE)
})

source(file.path(getwd(), '/posthocFunctions/makeAveragePlotTime.R'))
pth <- file.path(getwd(), "TMP")
birds <- c("CAWA", "OSFL", "RUBL")
scenarios <- c("climate", "fire", "vegetation")
shp <- prepInputs(url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV", 
                           destinationPath = pth, 
                           filename2 = "caribouArea2")
plt <- makeAveragePlotTime(dataFolder = pth,
                           birds = c("CAWA", "OSFL", 'RUBL'),
                           scenarios = scenarios, shp = shp)


library(SpaDES)
source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
pth <- checkPath(file.path(getwd(), "outputs/06DEC19/effectsRasters/"), create = TRUE)
library(raster)
listOfRasters <- list(cumulativeEffect_abs = list(CAWA = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run2/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedCAWAYear2100.tif"),
                                                          raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run2/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedCAWAYear2100.tif")),
                                             OSFL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run2/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedOSFLYear2100.tif"),
                                                          raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run2/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedOSFLYear2100.tif")),
                                             RUBL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run2/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedRUBLYear2100.tif"),
                                                          raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run2/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedRUBLYear2100.tif"))))

dRas <- makeDeltaRasters(listOfRasters = listOfRasters,
                       relativeDelta = FALSE,
                       outputFolder = pth,
                       lightLoad = TRUE,
                       overwrite = FALSE,
                       upload = TRUE,
                       folderID = list(cumulativeEffect_abs = "1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"))

listOfRasters <- list(cumulativeEffect2_abs = list(CAWA = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run3/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedCAWAYear2100.tif"),
                                                               raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run3/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedCAWAYear2100.tif")),
                                                  OSFL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run3/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedOSFLYear2100.tif"),
                                                               raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run3/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedOSFLYear2100.tif")),
                                                  RUBL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run3/birdPredictionsV4dynamic/run2_LandR_SCFMpredictedRUBLYear2100.tif"),
                                                               raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run3/birdPredictionsV6dynamic/run2_LandR.CS_fSpredictedRUBLYear2100.tif"))))

dRas <- makeDeltaRasters(listOfRasters = listOfRasters,
                         relativeDelta = FALSE,
                         outputFolder = pth,
                         lightLoad = TRUE,
                         overwrite = FALSE,
                         upload = TRUE,
                         folderID = list(cumulativeEffect2_abs = "1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"))

# listOfRasters <- list(cumulativeEffect1_abs = list(CAWA = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedCAWAYear2100.tif"),
#                                                                 raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run1/birdPredictionsV6dynamic/run1_LandR.CS_fSpredictedCAWAYear2100.tif")),
#                                                    OSFL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedOSFLYear2100.tif"),
#                                                                 raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run1/birdPredictionsV6dynamic/run1_LandR.CS_fSpredictedOSFLYear2100.tif")),
#                                                    RUBL = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedRUBLYear2100.tif"),
#                                                                 raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR.CS_fS/run1/birdPredictionsV6dynamic/run1_LandR.CS_fSpredictedRUBLYear2100.tif"))))
# 
# dRas <- makeDeltaRasters(listOfRasters = listOfRasters, 
#                          relativeDelta = FALSE,
#                          outputFolder = pth, 
#                          lightLoad = TRUE,
#                          overwrite = FALSE,
#                          upload = TRUE,
#                          folderID = list(cumulativeEffect1_abs = "1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"))

source('~/GitHub/NWT/posthocFunctions/createCumEffRasters.R')
createCumEffRasters()
