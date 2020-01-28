library(SpaDES)
library(reproducible)
library(data.table)
library(usefun)
library(future)
library(future.apply)
googledrive::drive_auth("tati.micheletti@gmail.com")
# BIRDS
comparisons <- list(climate = c("V4", "V6"),
                    vegetation = c("LandR_", "LandR.CS_"),
                    fire = c("fS", "SCFM"))
source('/mnt/data/Micheletti/NWT/posthocFunctions/makeDiffAnalysis2.R')
plts <- future_lapply(seq_along(comparisons), function(index){ #future_
  pl <- makeDiffAnalysis2(resultsFolder = file.path(getwd(), "outputs/PAPER"), # Should test makeDiffAnalysis2 in this one too!!
                         Run = c("run1", "run2", "run3", "run4", "run5"),
                         Year = c(seq(2011, 2091, by = 20), 2100),
                         typeOfSpecies = "bird",
                         SpeciesScenario = c("V4", "V6"),
                         comparisons = comparisons[index], 
                         writeRas = TRUE, 
                         overwrite = TRUE)
})
source(file.path(getwd(), '/posthocFunctions/makeAveragePlotTime.R'))
pth <- file.path(getwd(), "outputs/PAPER/effectsRasters/")
birds <- c("CAWA", "OSFL", "RUBL")
scenarios <- c("climate", "fire", "vegetation")
shp <- prepInputs(url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV", 
                  destinationPath = pth, 
                  filename2 = "caribouArea2")
plt <- makeAveragePlotTime(dataFolder = pth, 
                           years =  c(seq(2011, 2091, by = 20), 2100),
                           Species = birds,
                           scenarios = scenarios, shp = shp)

# CARIBOU
comparisons <- list(vegetation = c("LandR_", "LandR.CS_"),
                    fire = c("fS", "SCFM"))
source('/mnt/data/Micheletti/NWT/posthocFunctions/makeDiffAnalysis2.R')
plts <- lapply(seq_along(comparisons), function(index){ #future_
  pl <- makeDiffAnalysis2(resultsFolder = file.path(getwd(), "outputs/PAPER"),
                         Run = c("run1", "run2", "run3", "run4", "run5"),
                         typeOfSpecies = "caribou",
                         Species = "caribou",
                         Year = c(seq(2011, 2091, by = 20), 2100),
                         comparisons = comparisons[index], 
                         writeRas = TRUE,
                         overwrite = TRUE)
})
source(file.path(getwd(), '/posthocFunctions/makeAveragePlotTime.R'))
pth <- file.path(getwd(), "outputs/PAPER/effectsRasters/")
scenarios <- c("fire", "vegetation")
shp <- prepInputs(url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV", 
                           destinationPath = pth, 
                           filename2 = "caribouArea2")
plt <- makeAveragePlotTime(dataFolder = pth, 
                          years = c(seq(2011, 2091, by = 20), 2100),
                           Species = "caribou",
                           scenarios = scenarios, shp = shp)

pal <- RColorBrewer::brewer.pal(7, name = "Set3")
plot(shp, col = pal)
legend("right",   # location of legend
      legend = shp@data$OBJECTID, # categories or elements to render in the legend
      fill = pal) # color palette to use to fill objects in legend.
title("Polygons in the BCR6 NWT")


# <---- HERE (for the workshop), but first, re-run everything here with ALL 5 runs for all of them! Still need to fix below... Need to lapply to create these!
library(SpaDES)
source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
pth <- checkPath(file.path(getwd(), "outputs/PAPER/"))
runs <- paste0("run", seq(1, 5))
# species <- "bird"
# filename <- ""
CC <- c("LandR.CS_fS", "V6")
noCC <- c("LandR_SCFM", "V4")
yearToCompare <- 2100
# individualSpecies <- c("CAWA", "OSFL", "RUBL")

library(raster)

lapply(runs, function(RUN){
  
  listOfRasterPaths <- list(CAWA = stack(raster(file.path(pth, paste0(noCC[1], "/", RUN, "/birdPredictions", noCC[2], "/", #Path
                                                 paste0(RUN, "_", noCC[1],"predictedCAWAYear", yearToCompare,".tif")))), #filename
                    raster(file.path(pth, paste0(CC[1], "/", RUN, "/birdPredictions", CC[2], "/", #Path
                                                 paste0(RUN, "_", CC[1],"predictedCAWAYear", yearToCompare,".tif"))))),
       OSFL = stack(raster(file.path(pth, paste0(noCC[1], "/", RUN, "/birdPredictions", noCC[2], "/", #Path
                                                 paste0(RUN, "_", noCC[1],"predictedOSFLYear", yearToCompare,".tif")))), #filename
                    raster(file.path(pth, paste0(CC[1], "/", RUN, "/birdPredictions", CC[2], "/", #Path
                                                 paste0(RUN, "_", CC[1],"predictedOSFLYear", yearToCompare,".tif"))))),
       RUBL = stack(raster(file.path(pth, paste0(noCC[1], "/", RUN, "/birdPredictions", noCC[2], "/", #Path
                                                 paste0(RUN, "_", noCC[1],"predictedRUBLYear", yearToCompare,".tif")))), #filename
                    raster(file.path(pth, paste0(CC[1], "/", RUN, "/birdPredictions", CC[2], "/", #Path
                                                 paste0(RUN, "_", CC[1],"predictedRUBLYear", yearToCompare,".tif"))))))
  
  names(listOfRasterPaths) <- paste0("cumulativeEffect_", RUN)
  foldID <- list("1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM")
  names(foldID) <- paste0("cumulativeEffect_", RUN)  
  
  dRas <- makeDeltaRasters(listOfRasters = listOfRasters,
                           relativeDelta = FALSE,
                           outputFolder = file.path(pth, "effectsRasters"),
                           lightLoad = TRUE,
                           overwrite = FALSE,
                           upload = TRUE,
                           folderID = foldID)
})

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

assign(listOfRasters, list(cumulativeEffect_, RUN,_abs = list(CAWA = stack(raster("/mnt/data/Micheletti/NWT/outputs/06DEC19/LandR_SCFM/run1/birdPredictionsV4dynamic/run1_LandR_SCFMpredictedCAWAYear2100.tif"),
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

source('~/GitHub/NWT/posthocFunctions/createCumEffRasters.R')
createCumEffRasters()
