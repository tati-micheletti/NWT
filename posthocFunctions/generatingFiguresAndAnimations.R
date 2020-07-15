# Generating plots for NWT project

# FOLDERS: PAPER
LandR.CS_fS <- "1EJ7ym5TQp5JPmyWyYupjc9QXHNRUBQji"
LandR_SCFM <- "1H9Cs1L5mW186SxbRKqXiD_T1mL0jUM_j"

caribou <- "1Oz_DFqhOeIOl-nXEVGfYAssE68iCg40R"
birds <- "1ymCZq7cPfXB2hA6rDpRd6J3lYkioQJxZ"

# GOOGLE FOLDER SAM_RESULTS_JUNE
fl <- grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/PAPER/LandR_SCFM/run1", 
                                       full.names = TRUE), patterns = paste("RAS_", "Climate", sep = "|"),
                unwanted = ".aux")


lapply(fl, function(ras){
  googledrive::drive_upload(ras, path = googledrive::as_id(LandR_SCFM))
})

# ~~~~~~~~~~~~~~~~~ BIOMASS

library("usefulFuns")
library("LandR")
library("reproducible")
library("data.table")
library("raster")
# invisible(source("/mnt/data/Micheletti/NWT/posthocFunctions/disturbancePlotCaribou.R"))

CSfolder <- "outputs/PAPER/LandR_SCFM/run1"
typeSim <- "NonClimateSensitive"

leadVegType <- usefulFuns::plotLeadingVegetationType(dataPath = CSfolder, typeSim = typeSim, saveRAS = TRUE)
maxBiomass <- usefulFuns::plotVegetationBiomass(dataPath = CSfolder, typeSim = typeSim,
                                            saveRAS = TRUE, colNA = "white")
biomassPerSpecies <- totalBiomassPerSpecies(dataPath =  CSfolder, typeSim = typeSim, 
                                            proportional = FALSE, overstory = TRUE)
biomassPerSpeciesProp <- totalBiomassPerSpecies(dataPath =  CSfolder, typeSim = typeSim, 
                                                proportional = TRUE, overstory = TRUE)
biomassPerSpeciesAll <- totalBiomassPerSpecies(dataPath =  CSfolder, typeSim = typeSim, 
                                               proportional = FALSE, overstory = FALSE)
biomassPerSpeciesPropAll <- totalBiomassPerSpecies(dataPath =  CSfolder, typeSim = typeSim, 
                                                   proportional = TRUE, overstory = FALSE)
burnSumm <- plotBurnSummary(CSfolder, typeSim = typeSim, lastYear = 2100) # theObject = LandR.CS_fS$burnSummary # if we have the object
disturbPlot <- forestAgePlot(CSfolder, typeSim = typeSim)

# If burnsummary is not available, only if we have burnDT...
# createBurnSummary(folderData = CSfolder, typeSim = typeSim)

# source('/mnt/data/Micheletti/NWT/posthocFunctions/makeAllPlots.R') # NEVER RUN ALL REPETITIONS AT ONCE!
# run1 <- makeAllPlots(CSfolder = "", typeSim = "CS_run1") # OUTDATED!

# ~~~~~~~~~~~~~~~~~~~~ COMPARISON EDEhzhie # 
# TODO: This one will be done for paper2 

source('/mnt/data/Micheletti/NWT/posthocFunctions/compareAveragesInOutSHP.R')
shp <- "https://drive.google.com/open?id=1GA7hGslGEE1DGIMsD4Ou9duesS-eGbyZ"
plt <- compareAveragesInOutSHP(shp = shp, targetFile = "birdRTMEdehzhie.tif",
                               folder = "/mnt/data/Micheletti/NWT/outputs/30JUL19/", 
                               raster1Name = "richnessRaster",
                               years = c(2001, 2100), 
                               useSE = TRUE,
                               facetPlot = FALSE,
                               plotMedian = TRUE)
toUpload <- file.path("/mnt/data/Micheletti/NWT/outputs/30JUL19", "rasterEdehzie.tif")
file.exists(toUpload)
googledrive::drive_upload(toUpload, path = googledrive::as_id("1EmkeRatJdMjwxvJBQjYwLRF7jDrujy5G"))

# ~~~~~~~~~~~~~~~~~ CARIBOU POP GROWTH
# BEING GENERATED IN THE MODULE!
# plotCaribou <- plotCaribou(startTime = 2011,
#                                currentTime = 2100,
#                                endTime = 2100,
#                                predictedCaribou = NWT$predictedCaribou,
#                                yearSimulationStarts = 2001)

# ~~~~~~~~~~~~~~~~~ CARIBOU RSF
caribouArea1 <- prepInputs(url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO", 
                           destinationPath = tempdir(), filename2 = "caribouArea1")

# corrModel2011 <- makeCorrelationModel(caribouRSFRas = raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2011.tif"), 
#                                   birdDiversityRas = raster::stack("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/diverstiyIndices/diversityIndicesYear2011.tif"), 
#                                   studyArea = caribouArea1)

# TODO: This one will be done for paper4
corrModel2100 <- makeCorrelationModel(caribouRSFRas = raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2100.tif"), 
                                  birdDiversityRas = raster::stack("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/diverstiyIndices/diversityIndicesYear2100.tif"), 
                                  studyArea = caribouArea1)

writeRaster(corrModel2100, file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/coOccurrence2100"), 
            format = "GTiff")
googledrive::drive_upload(file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/coOccurrence2100.tif"),
                          path = googledrive::as_id("130r_99kfhLIJE0Mz1FjyhiZnfM7s7-v6"))

### FOR EDEHZIE
# TODO: This one will be done for paper4
ede <- raster::raster("/mnt/data/Micheletti/NWT/outputs/edehzhieRaster.tif")

corrModelEdeh2100 <- makeCorrelationModel(caribouRSFRas = raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2100.tif"), 
                                      birdDiversityRas = raster::stack("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/diverstiyIndices/diversityIndicesYear2100.tif"), 
                                      studyArea = ede)
corrModelEdeh2011 <- makeCorrelationModel(caribouRSFRas = raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2011.tif"), 
                                          birdDiversityRas = raster::stack("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/diverstiyIndices/diversityIndicesYear2011.tif"), 
                                          studyArea = ede)
lapply(c(2100, 2011), function(y){
  r <- get(paste0("corrModelEdeh", y))
  pth <- paste0("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/coOccurrenceEdehz", y, ".tif")
  writeRaster(r, pth, 
              format = "GTiff", overwrite = TRUE)
  googledrive::drive_upload(pth,
                            path = googledrive::as_id("130r_99kfhLIJE0Mz1FjyhiZnfM7s7-v6"))
})

# carib <- createCaribouGIFFromList(pathData = file.path(getwd(), "outputs/28MAR19/"), 
#                          uploadFiles = FALSE)
source("/mnt/data/Micheletti/NWT/posthocFunctions/meanRSFtime.R")
predRS_CS <- readRDS(file = "/mnt/data/Micheletti/NWT/outputs/08JUN19/RSF/RSF_CS_12JUN19") # Get the caribouCS simList
dtCS <- meanRSFtime(predictedPresenceProbability = predRS_CS$predictedPresenceProbability, 
                        scenario = "CS",
                        initialTime = 2001)
# Do the same for noCS
predRS_noCS <- readRDS(file = "/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/RSF/RSF_noCS_10JUN19_likelyCorrect") # Get the caribouCS simList
dtNoCS <- meanRSFtime(predictedPresenceProbability = predRS_noCS$predictedPresenceProbability, 
                    scenario = "noCS",
                    initialTime = 2001)

source("/mnt/data/Micheletti/NWT/posthocFunctions/booAvrgTimePlot.R")
avrRSTime <- booAvrgTimePlot(dtCS = dtCS, dtNoCS = dtNoCS, upload = FALSE, outputFolder = tempdir())

source("/mnt/data/Micheletti/NWT/posthocFunctions/RSFplot.R")
#noCS
NoCS_2001 <- RSFplot(ras = predRS_noCS$predictedPresenceProbability$Year2001$TaigaPlains$relativeSelection, 
                     upload = FALSE, 
                     writeReclasRas = FALSE, 
                     outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/RSF/", 
                     rasName = "RSF_NoCS_2001")

NoCS_2011 <- RSFplot(ras = predRS_noCS$predictedPresenceProbability$Year2011$TaigaPlains$relativeSelection, 
                     upload = FALSE, 
                     writeReclasRas = FALSE, 
                     outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/RSF/", 
                     rasName = "RSF_NoCS_2011")

NoCS_2100 <- RSFplot(ras = predRS_noCS$predictedPresenceProbability$Year2100$TaigaPlains$relativeSelectionTaigaPlains_Year2100, 
                     upload = FALSE, 
                     writeReclasRas = FALSE, 
                     outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/RSF/", 
                     rasName = "RSF_NoCS_2100")

# CS
CS_2001 <- RSFplot(ras = predRS_CS$predictedPresenceProbability$Year2001$TaigaPlains$relativeSelection, 
                     upload = FALSE, 
                     writeReclasRas = FALSE, 
                     outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19/RSF/", 
                     rasName = "RSF_CS_2001")

CS_2011 <- RSFplot(ras = predRS_CS$predictedPresenceProbability$Year2011$TaigaPlains$relativeSelection, 
                     upload = FALSE, 
                     writeReclasRas = FALSE, 
                     outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19/RSF/", 
                     rasName = "RSF_CS_2011")

CS_2021 <- RSFplot(ras = predRS_CS$predictedPresenceProbability$Year2011$TaigaPlains$relativeSelection, 
                   upload = FALSE, 
                   writeReclasRas = FALSE, 
                   outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19/RSF/", 
                   rasName = "RSF_CS_2011")

CS_2100 <- RSFplot(ras = predRS_CS$predictedPresenceProbability$Year2100$TaigaPlains$relativeSelection, 
                     upload = FALSE, 
                     writeReclasRas = FALSE, 
                     outputFolder = "/mnt/data/Micheletti/NWT/outputs/08JUN19/RSF/", 
                     rasName = "RSF_CS_2100")


# ~~~~~~~~~~~~~~~~~ BIRDS
# # ~~~~~~~~~~~~~~~~~BIRDS CHANGE TABLE

# [ FIX ] Still need to test this function with shp == NULL!
folderBirds <- "/mnt/data/Micheletti/NWT/outputs/09OCT19/LandR.CS_fS/run1/birdPredictions"
source('/mnt/data/Micheletti/NWT/posthocFunctions/bootstrapPercentChanges.R') # INTERNAL FUNCTIONS TO BE PUT IN usefulFuns!!! [ FIX ]
shp <- "https://drive.google.com/open?id=1GA7hGslGEE1DGIMsD4Ou9duesS-eGbyZ"
cacheFolder <- "/mnt/data/Micheletti/NWT/cache/"
SpaDES.core::setPaths(cachePath = cacheFolder)
boot <- bootstrapPercentChanges(folder = folderBirds, 
                                years = c(2011, 2100), sampleSize = 50, n = 2, shp = NULL) # FIGURE OUT WHY IT TAKES SO LONG TO SIMULATE!
saveRDS(object = boot, file = file.path(folderBirds, "speciesChangeTables.rds"))

# ~~~~~~~~~~~~~~~~~ summaryBirds

library("usefulFuns")

birdsFolder <- file.path(getwd(), CSfolder, 'birdPredictions/')

birds2011 <- usefulFuns::grepMulti(x = list.files(birdsFolder, 
                                      full.names = TRUE), patterns = c("predicted", "2011"))
birds2100 <- usefulFuns::grepMulti(x = list.files(birdsFolder, 
                                      full.names = TRUE), patterns = c("predicted", "2100"))
birds <- c(birds2011, birds2100)
library("data.table")
resultsBirdSpecies <- rbindlist(lapply(birds, function(ras){
  r <- raster::raster(ras)
  dt <- data.table(species = substrBoth(strng = substrBoth(strng = names(r), howManyCharacters = 12), howManyCharacters = 4, fromEnd = FALSE),
                   year = as.numeric(substrBoth(strng = names(r), howManyCharacters = 4)),
                   mean = mean(r[], na.rm = TRUE),
                   min = min(r[], na.rm = TRUE),
                   max = max(r[], na.rm = TRUE),
                   median = median(r[], na.rm = TRUE))
})
)
write.csv(x = resultsBirdSpecies, file = file.path(birdsFolder, "summaryBirds.csv"))
rm(resultsBirdSpecies)

species <- c("RUBL", "OSFL", "CAWA", "BBWA", "WEWP")

createBirdsGIFFromList(pathData = file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/birdPredictions/"), 
                       species = species, mainFileName = "CSpredicted", ysrName = c(seq(2011, 2100, by = 10), 2100),
                       version = "V6", uploadFiles = TRUE, whereToReport = "BCR6_NWT")

# WhichToUp <- c("/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.0_30MAR19.png",
#             "/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.50_30MAR19.png",
#             "/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.100_30MAR19.png",
#             "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.0_30MAR19.png",
#             "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.50_30MAR19.png",
#             "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.100_30MAR19.png")
# 
#   lapply(WhichToUp, function(each){
#     googledrive::drive_upload(each,
#                               path = as_id("1lhhIr_865VZI05mhiQ91iN8MfDGn5PV7"))
#   })

