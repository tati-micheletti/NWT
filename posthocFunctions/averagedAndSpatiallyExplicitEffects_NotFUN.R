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
# source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
pth <- checkPath(file.path(getwd(), "outputs/PAPER/"))
runs <- paste0("run", seq(1, 5))
CC <- c("LandR.CS_fS", "V6")
noCC <- c("LandR_SCFM", "V4")
yearToCompare <- 2100

library(raster)

listOfRasters <- lapply(runs, function(RUN){
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
  
  names(listOfRasterPaths) <- c("CAWA", "OSFL", "RUBL")
  return(listOfRasterPaths)
# browser()
})
names(listOfRasters) <- paste0("cumulativeEffect_", runs)

foldID <- list(rep("1ymCZq7cPfXB2hA6rDpRd6J3lYkioQJxZ", times = length(runs)))
names(foldID) <- paste0("cumulativeEffect_", runs)


  dRas <- makeDeltaRasters(listOfRasters = listOfRasters,
                           relativeDelta = FALSE,
                           outputFolder = file.path(pth, "effectsRasters"),
                           lightLoad = TRUE,
                           overwrite = FALSE,
                           upload = TRUE,
                           folderID = foldID)

source('~/GitHub/NWT/posthocFunctions/createCumEffRasters.R')
createCumEffRasters()
