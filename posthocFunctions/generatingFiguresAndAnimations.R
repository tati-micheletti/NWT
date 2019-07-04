# Generating plots for NWT project

# Source all common functions
# invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))
# invisible(sapply(X = list.files(file.path(getwd(), "posthocFunctions/"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files("/mnt/data/Micheletti/NWT/functions/", full.names = TRUE), FUN = source))

# GOOGLE FOLDER SAM_RESULTS_JUNE
fl <- grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/", full.names = TRUE), patterns = c("pixelGroupMap"))
lapply(fl, function(ras){
  googledrive::drive_upload(ras,
                            path = googledrive::as_id("1zDHy3JzN3BpNE0mm_iMOSrQqlI87tqTL"))
})

googledrive::drive_upload(file.path(folderPath, paste0("biomassMapStack_", simul, ".png")),
                          path = googledrive::as_id("1zDHy3JzN3BpNE0mm_iMOSrQqlI87tqTL"))

# ~~~~~~~~~~~~~~~~~ BIOMASS
NWT_CS <- readRDS("/mnt/data/Micheletti/NWT/outputs/08JUN19/NWT_CS_09JUN19")
NWT_noCS <- readRDS("/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/NWT_noCS_09JUN19")
NWT_CS_SCFM <- readRDS("/mnt/data/Micheletti/NWT/outputs/11JUN19_CS_SCFM/NWT_CS_SCFM13JUN19")


#~~~~~~~~# ATTENTION: These plots are possibly WRONG!! #~~~~~~~~#
# source("/mnt/data/Micheletti/NWT/posthocFunctions/biomassPerSpeciesYearGRAPH")
# bio_CS <- biomassPerSpeciesYearGRAPH(pathData = file.path("/mnt/data/Micheletti/NWT/outputs/08JUN19"), 
#                            times = list(start = 2001, end = 2100), 
#                            version = "V4_CS", overwriteRasters = TRUE, 
#                            uploadFiles = FALSE, whereToReport = "BCR6_NWT")
# 
# bio_noCS <- biomassPerSpeciesYearGRAPH(pathData = file.path("/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS"), 
#                                      times = list(start = 2001, end = 2100), 
#                                      version = "V4_noCS", overwriteRasters = TRUE, 
#                                      uploadFiles = FALSE, whereToReport = "BCR6_NWT")

# LEADING SP MAP

sppEquivCol <- "NWT"
data("sppEquivalencies_CA", package = "LandR")
# Make NWT spp equivalencies
# Popu_Tri == Popu_Bal in NWT
# Quer_mac in LandR needs to be Quer_Mac in NWT
sppEquivalencies_CA[, NWT := c(Abie_Bal = "Abie_Bal", 
                               Betu_Pap = "Betu_Pap", 
                               Lari_Lar = "Lari_Lar", 
                               Pice_Gla = "Pice_Gla",
                               Pice_Mar = "Pice_Mar", 
                               Pinu_Ban = "Pinu_Ban", 
                               # Pinu_Con = "Pinu_Con", 
                               # Popu_Bal = "Popu_Bal", 
                               Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
# Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.

sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                 palette = "Set3")

folder <- "29JUN19"#18JUN19_CS_SCFM" #"08JUN19"
simul <- "CS_SCFM"
folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder,"/")
cohorDataList <- bringObjectTS(path = folderPath, rastersNamePattern = "cohortData")
pixelGroupList <- bringObjectTS(path = folderPath, rastersNamePattern = "pixelGroupMap")

# MAX AGE
maxAge <- data.table::rbindlist(lapply(X = 1:length(cohorDataList), function(index){
  cohort <- cohorDataList[[index]]
  pixelGroup <- pixelGroupList[[index]]
  a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
  r <- rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
  return(list(meanAge = mean(r[], na.rm = TRUE),
              minAge = min(r[], na.rm = TRUE),
              maxAge = max(r[], na.rm = TRUE),
              medianAge = median(r[], na.rm = TRUE)))
}))
# AGE ~~~~~~~~~~~~~~~~
maxAgePlot <- lapply(X = c(1, length(cohorDataList)), function(index){
  cohort <- cohorDataList[[index]]
  pixelGroup <- pixelGroupList[[index]]
  a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
  r <- SpaDES.tools::rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
  return(r)
})
names(maxAgePlot) <- c("maxAgePlot2011", "maxAgePlot2100")
rng = range(c(getValues(maxAgePlot[[1]]), getValues(maxAgePlot[[2]])), na.rm = TRUE)
brks <- c(seq(min(rng), max(rng), by = 10),max(rng)) 
nb <- length(brks)-1
cols <- rev(heat.colors(nb))
par(mfrow=c(1,2))
plot(maxAgePlot[[1]], breaks=brks, col=cols, lab.breaks=brks, main='Max Age 2011', colNA = "grey85")
plot(maxAgePlot[[2]], breaks=brks, col=cols, lab.breaks=brks, main='Max Age 2100', colNA = "grey85")
plot(maxAgePlot[[1]], breaks=brks, col=cols, lab.breaks=brks, main='Max Age 2011') 
plot(maxAgePlot[[2]], breaks=brks, col=cols, lab.breaks=brks, main='Max Age 2100', legend = FALSE)

# BIOMASS ~~~~~~~~~~~~~~~~
maxBiomassPlot <- lapply(X = c(1, length(cohorDataList)), function(index){
  cohort <- cohorDataList[[index]]
  pixelGroup <- pixelGroupList[[index]]
  a <- cohort[, list(sumBio = sum(B, na.rm = TRUE)), by = "pixelGroup"]
  r <- SpaDES.tools::rasterizeReduced(a, pixelGroup, "sumBio", "pixelGroup")
  return(r)
})
names(maxBiomassPlot) <- c("maxBiomassPlot2011", "maxBiomassPlot2100")
rng = range(c(getValues(maxBiomassPlot[[1]]), getValues(maxBiomassPlot[[2]])), na.rm = TRUE)
brks <- seq(min(rng), max(rng)/10, by = (max(rng)/10-min(rng))/10) 
nb <- length(brks)-1
cols <- rev(heat.colors(nb))
par(mfrow=c(1,2))
plot(maxBiomassPlot[[1]], breaks=brks, col=cols, lab.breaks=brks, main='Max biomass 2011', colNA = "grey85")
plot(maxBiomassPlot[[2]], breaks=brks, col=cols, lab.breaks=brks, main='Max biomass 2100', colNA = "grey85")
plot(maxBiomassPlot[[1]], breaks=brks, col=cols, lab.breaks=brks, main='Max biomass 2011') 
plot(maxBiomassPlot[[2]], breaks=brks, col=cols, lab.breaks=brks, main='Max biomass 2100', legend = FALSE)

# LEADING TYPE ~~~~~~~~~~~~~~ 

leadingSpecies <- lapply(X = c(1, length(cohorDataList)-1), function(index){
  cohort <- cohorDataList[[index]]
  pixelGroup <- pixelGroupList[[index]]
  r <- LandR::vegTypeMapGenerator(cohortdata = cohort, 
                                  pixelGroupMap = pixelGroup, 
                                  vegLeadingProportion = 0, 
                                  colors = sppColorVect, 
                                  unitTest = FALSE)
  return(r)
})
names(leadingSpecies) <- c("leadingSpecies2011", "leadingSpecies2100")
legendNames <- levels(leadingSpecies[[1]])[[1]]$VALUE
cols <- levels(leadingSpecies[[1]])[[1]]$colors
par(mfrow=c(1,1))
plot(leadingSpecies[[1]], col=cols, main='Leading species 2011', legend = TRUE)
plot(leadingSpecies[[2]], col=cols, main='Leading species 2100', legend = FALSE)
legend("right", legend = legendNames, fill = cols, horiz = TRUE)


maxAge$years <- c(seq(2001, 2100, by = 10), 2100)
shortAge <- maxAge[years %in% c(2011, 2100)]

library("ggplot2")
agePlot <- ggplot2::ggplot(data = maxAge, aes(x = years)) +
  geom_ribbon(aes(ymin = 40, ymax = 60), alpha = 0.3, fill = "red") + # Old burn
  geom_ribbon(aes(ymin = 0, ymax = 40), alpha = 0.3, fill = "yellow") + # Recent burn
  geom_ribbon(aes(ymin = 60, ymax = maxAge), alpha = 0.3, fill = "green") + # No burn
  geom_line(aes(y = meanAge), size = 1.2) +
  geom_line(aes(y = medianAge), size = 1.2, linetype = "dashed") +
  ggtitle(paste0("Forest Age")) +
  theme(legend.position = "bottom")

tb <- data.table(years = c(seq(2001, 2100, by = 10), 2100),
                 index = 1:length(cohorDataList))
histAge <- data.table::rbindlist(lapply(X = 1:length(cohorDataList), function(index){
  cohort <- cohorDataList[[index]]
  pixelGroup <- pixelGroupList[[index]]
  a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
  r <- rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
  return(list(vals = r[!is.na(r)],
              index = index))
}))
histAge <- merge(histAge, tb)
histAge$color <- ifelse(histAge$vals < 40, "yellow", 
                        ifelse(histAge$vals >= 40 & histAge$vals <= 60, "red", "green"))

# histPlot <- ggplot2::ggplot(data = histAge) + 
#   facet_grid(rows = vars(years)) +
#   geom_histogram(binwidth=.1, aes(x = vals)) #+
#   # geom_ribbon(aes(xmin = 40, xmax = 60), alpha = 0.3, fill = "red") + # Old burn
#   # geom_ribbon(aes(xmin = 0, xmax = 40), alpha = 0.3, fill = "yellow") + # Recent burn
#   # geom_ribbon(aes(xmin = 60, xmax = max()), alpha = 0.3, fill = "green") + # No burn
#   
#   # scale_fill_discrete(palette = "RdYlGn", limits = c(40, 60, 0, 40, 40, 300))


leadingMaps <- data.table::rbindlist(lapply(X = 1:length(cohorDataList), function(index){
  browser()
  cohort <- cohorDataList[[index]]
  pixelGroup <- pixelGroupList[[index]]
  
  if (NROW(cohort[duplicated(cohort)]) != 0)
    cohort <- cohort[!duplicated(cohort)]
  pixelCohortData <- LandR::addNoPixel2CohortData(cohort, pixelGroup)
  pixelCohortData[, B := as.double(B)]
  thisPeriod <- pixelCohortData[, list(year = years[[index]],
                                       BiomassBySpecies = sum(B*noPixels, na.rm = TRUE)),
                                by = .(speciesCode)]
  
  # For proportional
  # thisPeriod$propBiomassBySpecies <- 100*(thisPeriod$BiomassBySpecies/sum(thisPeriod$BiomassBySpecies))
  return(thisPeriod)
})
)
png(filename = file.path(folderPath, paste0("biomassMapStack_", simul, ".png")), height = 600, width = 900)

plot2 <- ggplot(data = leadingMaps, aes(x = year, y = BiomassBySpecies,
                                       fill = speciesCode, group = speciesCode)) +
  geom_area(position = "stack") +
  # geom_line(aes(color = speciesCode)) +
  scale_fill_manual(values = sppColorVect) +
  labs(x = "Year", y = "Biomass") +
  theme(legend.text = element_text(size = 16), legend.title = element_blank(), 
        text = element_text(size=16),
        axis.text.x = element_text(size = 16))
quickPlot::clearPlot()
quickPlot::Plot(plot2, title = paste0("Total biomass by species\n",
                           "across pixels"), new = TRUE)

dev.off()

leadingMaps

library("ggplot2")
png(filename = file.path(folderPath, paste0("biomassMapStack_", simul, ".png")), height = 600, width = 900)

barPlots <- ggplot(data = leadingMaps, aes(x = year, y = BiomassBySpecies, fill = speciesCode), position = "fill") +
  geom_col() +
  scale_fill_viridis_d() +
  labs(x = "Year", y = "Total biomass") +
  theme_bw() +
  theme(legend.text = element_text(size = 20), legend.title = element_blank(), 
        text = element_text(size=20),
        axis.text.x = element_text(size = 20),
        title = element_text(size = 22))
quickPlot::clearPlot()
# quickPlot::Plot(barPlots, title = paste0("Total biomass by species\n",
#                                       "across pixels", new = TRUE)
print(barPlots)
dev.off()


# ~~~~~~~~~~~~~~~~~ CARIBOU POP GROWTH

plotCaribou <- plotCaribou2(startTime = 2011,
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

# ~~~~~~~~~~~~~~~~~ FIRE
source("/mnt/data/Micheletti/NWT/posthocFunctions/makeTimeSeriesPlots.R")
source("/mnt/data/Micheletti/NWT/posthocFunctions/compareSCFMPredictions.R")

firePredCS <- compareSCFMPredictions(polyList = names(NWT_CS$scfmDriverPars), simList = NWT_CS, scenario = "CS")
firePrednoCS <- compareSCFMPredictions(polyList = names(NWT_noCS$scfmDriverPars), simList = NWT_noCS, scenario = "noCS")
firePred <- rbind(firePredCS, firePrednoCS)

firePredCS_SCFM <- compareSCFMPredictions(polyList = names(NWT_CS_SCFM$scfmDriverPars), 
                                          simList = NWT_CS_SCFM, scenario = "CS_SCFM")

FIRE_CS <- FIRE_noCS <- FIRE_fS_LandR <- FIRE_SCFM_LandRCS <- NULL

FIRE_CS <- makeTimeSeriesPlots(path = "/mnt/data/Micheletti/NWT/outputs/08JUN19/", 
                    rastersNamePattern = "burnMap", scenario = "fS_LandR.CS")

FIRE_noCS <- makeTimeSeriesPlots(path = "/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/", 
                    rastersNamePattern = "burnMap", scenario = "SCFM_LandR")

FIRE_fS_LandR <- makeTimeSeriesPlots(path = "/mnt/data/Micheletti/NWT/outputs/11JUN19_noCS_fS/", 
                               rastersNamePattern = "burnMap", scenario = "fS_LandR")

FIRE_SCFM_LandRCS <- makeTimeSeriesPlots(path = "/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/", 
                                     rastersNamePattern = "burnMap", scenario = "SCFM_LandR.CS")


burnedArea <- rbind(FIRE_CS$burnedArea, 
                    FIRE_noCS$burnedArea, 
                    FIRE_fS_LandR$burnedArea, 
                    FIRE_SCFM_LandRCS$burnedArea)

library(ggplot2)
p <- ggplot(burnedArea, aes(x = year, y = burned, colour = scenario)) +
  geom_line()
p

nFires <- NWT_CS_SCFM$burnSummary[, .N, by = year]

fires <- ggplot(nFires, aes(x = year, y = N)) +
  geom_line()
fires

# ~~~~~~~~~~~~~~~ CUM BURN 
# 
# CS
CFIRE_CS <- makeTimeSeriesPlots(path = "/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/", 
                               rastersNamePattern = "rstCurrentBurn_year", scenario = "CS_SCFM")

# no CS
CFIRE_noCS <- makeTimeSeriesPlots(path = "/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/", 
                                 rastersNamePattern = "", scenario = "noCS")

CburnedArea <- rbind(CFIRE_CS$burnedArea, CFIRE_noCS$burnedArea)

library(ggplot2)
p2 <- ggplot(CFIRE_CS$burnedArea, aes(x = year, y = burned, colour = scenario)) +
  geom_line()
p2

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

