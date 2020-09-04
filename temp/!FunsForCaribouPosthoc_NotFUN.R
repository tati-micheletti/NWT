# functions for caribou posthoc -- migrated from ~/posthocFunctions/averagedAndSpatiallyExplicitEffects_NotFUN.R"

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

library(SpaDES)
pth <- checkPath(file.path(getwd(), "outputs/PAPER/"))
runs <- paste0("run", seq(1, 5))
CC <- c("LandR.CS_fS", "V6")
noCC <- c("LandR_SCFM", "V4")
yearToCompare <- 2100

# CARIBOU!!
CC <- c("LandR.CS_fS")
noCC <- c("LandR_SCFM")

listOfRasters <- lapply(runs, function(RUN){
  listOfRasterPaths <- list(caribou = stack(raster(file.path(pth, paste0(noCC, "/", RUN, "/caribouPredictions"), #Path
                                                             paste0("relativeSelectionTaigaPlains_Year", yearToCompare,".tif"))), #filename
                                            raster(file.path(pth, paste0(CC, "/", RUN, "/caribouPredictions"), #Path
                                                             paste0("relativeSelectionTaigaPlains_Year", yearToCompare,".tif")))))
  listOfRasterPaths <- lapply(listOfRasterPaths, function(ras){
    names(ras) <- c(paste0("caribou_", noCC), paste0("caribou_", CC))
    return(ras)
  })
  names(listOfRasterPaths) <- c("caribou")
  return(listOfRasterPaths)
})
names(listOfRasters) <- paste0("cumulativeEffect_", runs)

foldID <- as.list(c(rep("1Oz_DFqhOeIOl-nXEVGfYAssE68iCg40R", times = 5)))
names(foldID) <- paste0("cumulativeEffect_", runs)

source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
dRas <- makeDeltaRasters(listOfRasters = listOfRasters,
                         relativeDelta = FALSE,
                         outputFolder = file.path(pth, "effectsRasters"),
                         lightLoad = TRUE,
                         overwrite = FALSE,
                         upload = TRUE,
                         folderID = foldID,
                         email = "tati.micheletti@gmail.com")

source('/mnt/data/Micheletti/NWT/posthocFunctions/createCumEffRasters.R')
createCumEffRasters(species = c("caribou"),
                    rasFolder = "/mnt/data/Micheletti/NWT/outputs/PAPER/effectsRasters",
                    googlefolderID = "1Oz_DFqhOeIOl-nXEVGfYAssE68iCg40R")
