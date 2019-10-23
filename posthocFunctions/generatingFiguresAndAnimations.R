# Generating plots for NWT project

# GOOGLE FOLDER SAM_RESULTS_JUNE
fl <- usefun::grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed", 
                                       full.names = TRUE), patterns = c("predicted"))
lapply(fl, function(ras){
  googledrive::drive_upload("/mnt/data/Micheletti/NWT/outputs/30JUL19//comparisonrelative.png",
                            path = googledrive::as_id("1ZAT58duMpetvoweR6yHBnXDDhbEuQAVP"))
})
# 1rLON4rDbvjbq-hHnAqFVXvehPQ-AcOkj
# googledrive::drive_upload(file.path(folderPath, paste0("biomassMapStack_", simul, ".png")),
#                           path = googledrive::as_id("1zDHy3JzN3BpNE0mm_iMOSrQqlI87tqTL"))

# ~~~~~~~~~~~~~~~~~ BIOMASS
# NWT_CS <- readRDS("/mnt/data/Micheletti/NWT/outputs/14JUL19/NWT_CS_fS_14JUL19")
# NWT_noCS <- readRDS("/mnt/data/Micheletti/NWT/outputs/12JUL19/NWT_noCS_fS13JUL19")

# noCS <- "12JUL19"
# CS <- "14JUL19", "18JUL19/run1", "18JUL19/run2", "10JUL19"?

library("usefun")
library("LandR")
library("reproducible")
library("data.table")
library("raster")
invisible(lapply(paste0("/mnt/data/Micheletti/NWT/posthocFunctions/", c("plotMaxAge.R",
                                                              "plotVegetationBiomass.R",
                                                              "plotLeadingVegetationType.R",
                                                              "totalBiomassPerSpecies.R",
                                                              "plotBurnSummary.R",
                                                              "disturbancePlotCaribou.R")), source))
# 
CSfolder <- "outputs/09OCT19/LandR.CS_fS/run1"
typeSim <- "CS"

# biomassPerSpecies_1 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "1"), typeSim = paste0(typeSim, "1"))
# biomassPerSpecies_2 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "2"), typeSim = paste0(typeSim, "2"))
# biomassPerSpecies_3 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "3"), typeSim = paste0(typeSim, "3"))
# biomassPerSpecies_4 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "4"), typeSim = paste0(typeSim, "4"))
# biomassPerSpecies_5 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "5"), typeSim = paste0(typeSim, "5"))
# biomassPerSpecies_6 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "6"), typeSim = paste0(typeSim, "6"))
# biomassPerSpecies_7 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "7"), typeSim = paste0(typeSim, "7"))
# biomassPerSpecies_8 <- totalBiomassPerSpecies(folderData = paste0(CSfolder, "8"), typeSim = paste0(typeSim, "8"))

leadVegType <- plotLeadingVegetationType(folderData = CSfolder, typeSim = typeSim, years = c(seq(2001, 2091, by = 10), 2100))
maxAge <- plotMaxAge(folderData = CSfolder, typeSim = typeSim, years = c(seq(2001, 2091, by = 10), 2100))
maxBiomass <- plotVegetationBiomass(folderData = CSfolder, typeSim = typeSim, years = c(seq(2001, 2091, by = 10), 2100))
biomassPerSpecies <- totalBiomassPerSpecies(folderData = CSfolder, years = c(seq(2001, 2091, by = 10), 2100),
                                                typeSim = typeSim)
biomassPerSpeciesProp <- totalBiomassPerSpecies(folderData = CSfolder, years = c(seq(2001, 2091, by = 10), 2100),
                                                    typeSim = typeSim, proportional = TRUE)
burnSumm <- plotBurnSummary(CSfolder, typeSim = typeSim, lastYear = 2100) # theObject = LandR.CS_fS$burnSummary # if we have the object
disturbPlot <- disturbancePlotCaribou(CSfolder, typeSim = typeSim)

# If burnsummary is not available, only if we have burnDT...
# createBurnSummary(folderData = CSfolder, typeSim = typeSim)

# source('/mnt/data/Micheletti/NWT/posthocFunctions/makeAllPlots.R') # NEVER RUN ALL REPETITIONS AT ONCE!
# run7 <- makeAllPlots(CSfolder = "30JUL19/run7", typeSim = "CS_run7")

# ~~~~~~~~~~~~~~~~~~~~ COMPARISON EDEhzhie

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

# ~~~~~~~~~~~~~~~~~BIRDS CHANGE TABLE

# [ FIX ] Still need to test this function with shp == NULL!
folderBirds <- "/mnt/data/Micheletti/NWT/outputs/09OCT19/LandR.CS_fS/run1/birdPredictions"
source('/mnt/data/Micheletti/NWT/posthocFunctions/bootstrapPercentChanges.R') # INTERNAL FUNCTIONS TO BE PUT IN USEFUN!!! [ FIX ]
shp <- "https://drive.google.com/open?id=1GA7hGslGEE1DGIMsD4Ou9duesS-eGbyZ"
cacheFolder <- "/mnt/data/Micheletti/NWT/cache/"
SpaDES.core::setPaths(cachePath = cacheFolder)
boot <- bootstrapPercentChanges(folder = folderBirds, 
                                    years = c(2011, 2091), sampleSize = 50, n = 2, shp = NULL) # FIGURE OUT WHY IT TAKES SO LONG TO SIMULATE!
saveRDS(object = boot, file = file.path(folderBirds, "speciesChangeTables.rds"))
# ~~~~~~~~~~~~~~~~~ CARIBOU POP GROWTH

plotCaribou <- plotCaribou(startTime = 2011,
                               currentTime = 2100,
                               endTime = 2100,
                               predictedCaribou = NWT$predictedCaribou,
                               yearSimulationStarts = 2001)

# ~~~~~~~~~~~~~~~~~ CARIBOU
caribouArea1 <- prepInputs(url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO", 
                           destinationPath = tempdir(), filename2 = "caribouArea1")
# 
# corrModel2011 <- makeCorrelationModel(caribouRSFRas = raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2011.tif"), 
#                                   birdDiversityRas = raster::stack("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/diverstiyIndices/diversityIndicesYear2011.tif"), 
#                                   studyArea = caribouArea1)

corrModel2100 <- makeCorrelationModel(caribouRSFRas = raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2100.tif"), 
                                  birdDiversityRas = raster::stack("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/diverstiyIndices/diversityIndicesYear2100.tif"), 
                                  studyArea = caribouArea1)

writeRaster(corrModel2100, file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/coOccurrence2100"), 
            format = "GTiff")
googledrive::drive_upload(file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/coOccurrence2100.tif"),
                          path = googledrive::as_id("130r_99kfhLIJE0Mz1FjyhiZnfM7s7-v6"))

### FOR EDEHZIE

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
library("usefun")
birdsFolder <- file.path(getwd(), CSfolder, 'birdPredictions/')
birds2011 <- usefun::grepMulti(x = list.files(birdsFolder, 
                                      full.names = TRUE), patterns = c("predicted", "2011"))
birds2091 <- usefun::grepMulti(x = list.files(birdsFolder, 
                                      full.names = TRUE), patterns = c("predicted", "2091"))
birds <- c(birds2011, birds2091)
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

species <- c("RUBL", "OSFL", "CAWA")

createBirdsGIFFromList(pathData = file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/birdPredictions/"), 
                       species = species, mainFileName = "CSpredicted", ysrName = c(seq(2011, 2100, by = 10), 2100),
                       version = "V6", uploadFiles = TRUE, whereToReport = "BCR6_NWT")

WhichToUp <- c("/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.0_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.50_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.100_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.0_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.50_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.100_30MAR19.png")

  lapply(WhichToUp, function(each){
    googledrive::drive_upload(each,
                              path = as_id("1lhhIr_865VZI05mhiQ91iN8MfDGn5PV7"))
  })

