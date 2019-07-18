# Generating plots for NWT project

# GOOGLE FOLDER SAM_RESULTS_JUNE
# fl <- grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/", full.names = TRUE), patterns = c("pixelGroupMap"))
# lapply(fl, function(ras){
#   googledrive::drive_upload(ras,
#                             path = googledrive::as_id("1zDHy3JzN3BpNE0mm_iMOSrQqlI87tqTL"))
# })
# 
# googledrive::drive_upload(file.path(folderPath, paste0("biomassMapStack_", simul, ".png")),
#                           path = googledrive::as_id("1zDHy3JzN3BpNE0mm_iMOSrQqlI87tqTL"))

# ~~~~~~~~~~~~~~~~~ BIOMASS
NWT_CS <- readRDS("/mnt/data/Micheletti/NWT/outputs/14JUL19/NWT_CS_fS_14JUL19")
NWT_noCS <- readRDS("/mnt/data/Micheletti/NWT/outputs/12JUL19/NWT_noCS_fS13JUL19")

library("usefun")
library("LandR")
library("reproducible")
invisible(lapply(paste0("/mnt/data/Micheletti/NWT/posthocFunctions/", c("plotMaxAge.R", 
                                                              "plotVegetationBiomass.R",
                                                              "plotLeadingVegetationType.R",
                                                              "totalBiomassPerSpecies.R",
                                                              "totalBiomassPerSpecies.R",
                                                              "createBurnSummary.R",
                                                              "disturbancePlotCaribou.R")), source))
noCSfolder <- "12JUL19"
maxAge_noCS <- plotMaxAge(folderData = noCSfolder, typeSim = "noCS")
maxBiomass_noCS <- plotVegetationBiomass(folderData = noCSfolder, typeSim = "noCS")
leadVegType_noCS <- plotLeadingVegetationType(folderData = noCSfolder, typeSim = "noCS")
biomassPerSpecies_noCS <- totalBiomassPerSpecies(folderData = noCSfolder, 
                                            typeSim = "noCS")
biomassPerSpeciesProp_noCS <- totalBiomassPerSpecies(folderData = noCSfolder, 
                                            typeSim = "noCS", proportional = TRUE)
burnSumm_noCS <- createBurnSummary(noCSfolder, typeSim = "noCS")
disturbPlot_noCS <- disturbancePlotCaribou(noCSfolder, typeSim = "noCS")


CSfolder <- "17JUL19"
maxAge_CS1 <- plotMaxAge(folderData = CSfolder, typeSim = "CS")
maxBiomass_CS1 <- plotVegetationBiomass(folderData = CSfolder, typeSim = "CS")
leadVegType_CS1 <- plotLeadingVegetationType(folderData = CSfolder, typeSim = "CS")
biomassPerSpecies_CS1 <- totalBiomassPerSpecies(folderData = CSfolder, 
                                            typeSim = "CS")
biomassPerSpeciesProp_CS1 <- totalBiomassPerSpecies(folderData = CSfolder, 
                                               typeSim = "CS", proportional = TRUE)
burnSumm_CS1 <- plotBurnSummary(CSfolder, typeSim = "CS")
disturbPlot_CS1 <- disturbancePlotCaribou(CSfolder, typeSim = "CS")

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

birds2011 <- grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/birdPredictions/", 
                                      full.names = TRUE), patterns = c("predicted", "2011"))
birds2100 <- grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/birdPredictions/", 
                                      full.names = TRUE), patterns = c("predicted", "2100"))
birds <- c(birds2011, birds2100)
resultsBirdSpecies <- rbindlist(lapply(birds, function(ras){
  r <- raster::raster(ras)
  dt <- data.table(species = substrBoth(substrBoth(x = names(r), n = 12), n = 8, fromEnd = FALSE),
                   year = as.numeric(substrBoth(x = names(r), n = 4)),
                   mean = mean(r[], na.rm = TRUE),
                   min = min(r[], na.rm = TRUE),
                   max = max(r[], na.rm = TRUE),
                   median = median(r[], na.rm = TRUE))
})
)

write.csv(x = resultsBirdSpecies, file = "/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/summaryBirds.csv")

species <- c("RUBL", "OSFL", "CAWA")

createBirdsGIFFromList(pathData = file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/birdPredictions/"), 
                       species = species, mainFileName = "CSpredicted", ysrName = c(seq(2011, 2100, by = 10), 2100),
                       version = "V4", uploadFiles = TRUE, whereToReport = "BCR6_NWT")

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

