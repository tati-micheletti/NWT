#########################################################
##          P L O T S    C A R I B O U                 ##
#########################################################

# RUN SOURCE UNTIL script 2 

resultsFolder <- file.path(getwd(), "outputs/landscapeRuns/LandR.CS_fS")

SpaDES.core::setPaths(cachePath = posthocCache,
                      outputPath = checkPath(file.path(resultsFolder, 
                                                       "caribouPlots"),
                                             create = TRUE))
# Make Caribou RSF map
# This will be a difference (2100-2011) map, averaged across all reps
# and then across all simulations. Need also to summarize by polygons and
# return in a table

binningTable <- Cache(prepInputs,
                      targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                      url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
                      destinationPath = Paths$outputPath, 
                      fun = "data.table::fread",
                      userTags = c("object:binningTable", "goal:finalPlots"))

# Fixing the caribou SHP to use 
# Decho N, Decho S, Hay river lowlands, GSA N, and GSA S

booSHP <- objects$listSACaribou$metaHeards[objects$listSACaribou$metaHeards$HERD %in% 
                                             c("Dehcho South_v2", "Dehcho North_v2", 
                                               "Hay River Lowlands", "GSA South", "GSA North"),]
Require("RColorBrewer")
source('posthocFunctions/convertShpToRas.R')
source('posthocFunctions/makeCaribouRSFAverageMap.R')
booRSF <- makeCaribouRSFAverageMap(resultsFolder = resultsFolder,
                                   runs = paste0("run", 1:5), 
                                   climateModels = c("CCSM4", "CanESM2", "INM-CM4"), 
                                   outputsPath = Paths$outputPath,
                                   shp = booSHP,
                                   binningTable = binningTable,
                                   initialYear = 2011,
                                   lastYear = 2100)

meanPolys <- qs::qread("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/meanRSFperPolygon.qs")
Require("Rmisc")
meanPolys[, upperCI := Rmisc::CI(x = RSF, ci = 0.95)[["upper"]], by = c("Area", "climateModel")]
meanPolys[, lowerCI := Rmisc::CI(x = RSF, ci = 0.95)[["lower"]], by = c("Area", "climateModel")]
meanPolys[, sdRSF := sd(RSF), by = c("Area", "climateModel")]
meanPolys[, sdRSFall := sd(RSF), by = c("Area")]
meanPolys[, meanRSFall := mean(RSF), by = c("Area")]
meanPolys[, upperCIall := Rmisc::CI(x = RSF, ci = 0.95)[["upper"]], by = "Area"]
meanPolys[, lowerCIall := Rmisc::CI(x = RSF, ci = 0.95)[["lower"]], by = "Area"]

sMP <- unique(meanPolys[, c("Area","climateModel", "meanRSF", "upperCI", "lowerCI", 
                            "sdRSF", 
                            "meanRSFall",
                            "sdRSFall", "upperCIall","lowerCIall") ])
qs::qsave(x = sMP, file = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/meanRSFperPolygonSummary.qs")
library("googledrive")
drive_upload("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/meanRSFperPolygonSummary.qs", path = as_id("1toWGstfAdu3fB7nzh3tTO_dZXjQbSHts"))

source('~/projects/NWT/posthocFunctions/makeCaribouAverageTS.R')
library("tictoc")
booTS <- makeCaribouAverageTS(climateScenarios = c("CCSM4", "CanESM2", 
                                                   "INM-CM4"),
                              years = c(seq(2011, 2091, by = 20), 2100),
                              reps = paste0("run", 1:5),
                              pathData = dirname(Paths$outputPath),
                              pathOutputs = Paths$outputPath,
                              overwriteRas = FALSE,
                              pathInputs = Paths$inputPath,
                              binningTable = binningTable,
                              overwritePNG = TRUE)

pathOutputs <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/"
source('~/projects/NWT/posthocFunctions/plotCaribouPopGrowthMS.R')
plotCaribou <- plotCaribouPopGrowthMS(startTime = 2011,
                                      currentTime = 2100,
                                      endTime = 2100,
                                      climateModel = c("CCSM4", "CanESM2", "INM-CM4"),
                                      reps = paste0("run", 1:5),
                                      resultsMainFolder = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                                      yearSimulationStarts = 2011,
                                      whichPolys = c("Bistcho", "Maxhamish", "Yates"),
                                      outputFolder = pathOutputs)
library("googledrive")
fl <- list.files(path = pathOutputs, pattern = "caribou_", full.names = TRUE)
lapply(fl, drive_upload, path = as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))

# Digest disturbance
allDist <- list.files(resultsFolder, pattern = "disturbances_year2100", 
                      full.names = TRUE, recursive = TRUE)
allcombs <- data.table(expand.grid(c("CCSM4_", "CanESM2_", "INM-CM4_"), paste0("run", 1:5)))
allcombs[, comb := paste0(Var1, Var2)]
distTable <- rbindlist(lapply(allcombs$comb, function(distFile){
  fl <- allDist[grep(pattern = distFile, x = allDist)]
  Mod <- strsplit(distFile, split = "_")[[1]][1]
  Run <- strsplit(distFile, split = "_")[[1]][2]
  listDT <- readRDS(fl)
  DT <- rbindlist(lapply(names(listDT), function(Year){
    yearList <- rbindlist(lapply(names(listDT[[Year]]), function(Area){
      areaList <- rbindlist(lapply(names(listDT[[Year]][[Area]]), function(Polygon){
        polyList <- data.table(listDT[[Year]][[Area]][[Polygon]])
        polyList[, c("Year", "Area", "Polygon") := list(as.numeric(
          usefulFuns::substrBoth(strng = Year, 
                                 howManyCharacters = 4, 
                                 fromEnd = TRUE)),
          Area, Polygon)]
        return(polyList)
      }))
      return(areaList)
    }))
    return(yearList)
  }))
  DT[, c("run", "Model") := list(Run, Mod)]
  return(DT)
}))
Require("qs")
qs::qsave(x = distTable, file = file.path(file.path(resultsFolder, 
                                                    "caribouPlots"), "disturbanceTable_3Scenarios.qs"))

Require("googledrive")
drive_upload(file.path(file.path(resultsFolder, 
                                 "caribouPlots"), "disturbanceTable_3Scenarios.qs"), 
             path = as_id("1m-fkwxbWIONWSl0j9sGRQHzsjVQOHab8"))

# Vegetation
library("tictoc")
tic("Total time elapsed for biomass plots: ")
source('~/projects/NWT/posthocFunctions/biomassPlotsCaribou.R')
shpPoly <- maptools::readShapeLines("~/projects/NWT/inputs/NWT_NT1_BCR6_2011/RSFshp.shp")

pl <- biomassPlotsCaribou(years = c(2011, 2100),
                          pathData = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                          pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                          Scenarios = c("CCSM4",
                                        "CanESM2",
                                        "INM-CM4"),
                          shpPoly = shpPoly,
                          flammableRTM = rasterToMatch,
                          runs = paste0("run", 1:5))
toc()

tic("Total time elapsed for leading plots: ")
source('~/projects/NWT/posthocFunctions/leadingSpPlotsCaribou.R')
pl2 <- leadingSpPlotsCaribou(leadingPercentage = 0.75,
                      years = c(2011, 2100),
                      pathData = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                      pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                      Scenarios = c("CCSM4",
                                    "CanESM2",
                                    "INM-CM4"),
                      runs = paste0("run", 1:5),
                      rasterToMatch = rasterToMatch,
                      flammableRTM = flammableRTM)
toc()

# Fire
source('~/projects/NWT/posthocFunctions/plotBurnSummaryRepsCaribou.R')
require("Require")
Require("ggplot2")
Require("gridExtra")
Require("data.table")
dtPath <-"~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/"
burnPlotCCSM <- plotBurnSummaryRepsCaribou(dataPath = dtPath,
                                typeSim = "CCSM4",
                                lastYear = 2100,
                                overwrite = TRUE)
burnPlotINM <- plotBurnSummaryRepsCaribou(dataPath = dtPath,
                                           typeSim = "INM-CM4",
                                           lastYear = 2100,
                                           overwrite = TRUE)
burnPlotCanESM2 <- plotBurnSummaryRepsCaribou(dataPath = dtPath,
                                           typeSim = "CanESM2",
                                           lastYear = 2100,
                                           overwrite = TRUE)
burnPlotAllScenarios <- plotBurnSummaryRepsCaribou(dataPath = dtPath,
                                              typeSim = c("CanESM2", "INM-CM4", 
                                                          "CCSM4"),
                                              lastYear = 2100,
                                              overwrite = TRUE)
# Upload all files:
library(googledrive)
fold <- as_id("1m-fkwxbWIONWSl0j9sGRQHzsjVQOHab8")
fireFiles <- list.files(path = dtPath, pattern = "burnSummary", 
                        full.names = TRUE)
vegFiles <- list.files(path = file.path(dtPath, "vegetationPlots"), 
                       pattern = "csv|png", full.names = TRUE)
landFiles <- c(fireFiles, vegFiles)
booFiles <- list.files(path = file.path(dtPath, "caribouPlots"), 
                       pattern = "qs|png|csv", full.names = TRUE)
booFolder <- as_id("1FWflq45Sqv0x90C0nug103Vjzhxv9_P4")
landFolder <- as_id("18V3xTu4tC9VWi9xp-F8ifxf-J3xP7YvS")
lapply(landFiles, drive_upload, path = landFolder)
lapply(booFiles, drive_upload, path = booFolder)
# 
# drive_update("TEST.txt", as_id("1toWGstfAdu3fB7nzh3tTO_dZXjQbSHts"))

############ 
############ GET CARIBOU POP GROWTH DATA
############ 
source('~/projects/NWT/posthocFunctions/extractPopGrowthData.R')
pathOutputs <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/"
popGrowthTable <- extractPopGrowthData(currentTime = 2100,
                                      climateModel = c("CCSM4", "CanESM2", "INM-CM4"),
                                      resultsMainFolder = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                                      whichPolys = c("Dehcho South_v2", "Dehcho North_v2", 
                                                     "Hay River Lowlands", "GSA South", "GSA North"),
                                      outputFolder = pathOutputs)

popGrowthTableSim <- unique(popGrowthTable[, c("Polygon", "femSurvMod_recrMod", 
                                               "climateModel", "Year", "minRib", 
                                               "maxRib", "averageannualLambda")])
popGrowthTableSim[, meanLambda := mean(averageannualLambda), 
                  by = c("Polygon", "femSurvMod_recrMod", "Year")]

popGrowthTableSim[, minLambda := min(minRib), 
                  by = c("Polygon", "femSurvMod_recrMod", "Year")]

popGrowthTableSim[, maxLambda := max(maxRib), 
                  by = c("Polygon", "femSurvMod_recrMod", "Year")]

popGrowthTableSim2 <- unique(popGrowthTableSim[, c("Polygon", "femSurvMod_recrMod",
                                                   "Year", "minLambda", 
                                                   "maxLambda", "meanLambda")])
write.csv(popGrowthTableSim2, file.path(pathOutputs, "lambdaTable.csv"))

#  To see the difference of means between 2011 and 2100
popGrowthTableSim3 <- dcast(data = popGrowthTableSim2, formula = Polygon + femSurvMod_recrMod ~ Year, value.var = "meanLambda")
popGrowthTableSim3[, Diff := `2100`-`2011`]
popGrowthTableSim3[femSurvMod_recrMod == "Johnson_M4_National::Johnson_M4_National", c("Polygon", "2011", "2100")]

Require("googledrive")
drive_upload(media = file.path(pathOutputs, "lambdaTable.csv"), 
             path = as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))

#### Checking Boo Static vs Dynamic

booStatic <- raster::raster("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS_with2001/CCSM4_run1/caribouPredictions/relativeSelectioncaribouRSF_NT_Year2017_static.tif")

booDynamic <- raster::raster("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/CCSM4_run1/caribouPredictions/relativeSelectioncaribouRSF_NT_Year2017.tif")

# Apply the binning to both
binningTable <- Cache(prepInputs,
                      targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                      url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
                      destinationPath = tempdir(), 
                      fun = "data.table::fread",
                      userTags = c("object:binningTable", "goal:finalPlots"))

reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
                              binningTable[["Max.Value"]],
                              binningTable[["RSF.Bin"]]), 
                        ncol = 3)
# Make sure that the max value is infinite, so it accommodates any bigger value
# than before
reclassMatrix[nrow(reclassMatrix), 2] <- Inf
booStaticBin <- raster::reclassify(x = booStatic, rcl = reclassMatrix)
booDynamicBin <- raster::reclassify(x = booDynamic, rcl = reclassMatrix)
diffClass <- booDynamicBin-booStaticBin

# hist(diffClass, xlab = "RSF Bins", main = "",
#      ylab = "Frequency of pixels (x 10\u2076)", 
#      xlim = c(-9, 9))

tb <- table(diffClass[])
tbReady <- data.table(xAxis = factor(rownames(tb), 
                                     levels = as.character(-9:9)),
                 yAxis = as.numeric(tb))
palH <- colorRampPalette(RColorBrewer::brewer.pal(length(-9:9), name = "Spectral"))(length(-9:9))
ph <- ggplot(tbReady, aes(x = xAxis, y = yAxis/(10^6))) +
  xlab("RSF Bins") +
  ylab("Frequency of pixels (x 10\u2076)") +
  geom_col(fill = palH, col = "black") + 
  theme_classic(base_size = 16)
ph

ggsave(file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots/histogramComparison.png"), 
       width = 12,
       height = 8)


library("lattice")
library("rasterVis")
library("viridis")
library("maptools")

shpLoaded <- maptools::readShapeLines("~/projects/NWT/inputs/NWT_NT1_BCR6_2011/RSFshp.shp")

# Add shp to levelplot
nb.cols <- length(-9:9)
Pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))(nb.cols)
  Ras <- round(diffClass, 0)
  rasBinned <- ratify(Ras)
  att <- "ID"
  
  png(filename = file.path(getwd(), "outputs/caribou/differenceRSFMaps.png"),
      width = 21, height = 29,
      units = "cm", res = 300)
  
  print(rasterVis::levelplot(rasBinned,
                           sub = paste0("Differences in caribou RSF predictions using ",
                                        "static and dynamic land cover"),
                           att = att,
                           margin = FALSE,
                           maxpixels = 6e6,
                           colorkey = list(
                             space = 'bottom',
                             at = -9:9,
                             axis.line = list(col = 'black'),
                             width = 0.75
                           ),
                           par.settings = list(
                             strip.border = list(col = 'transparent'),
                             strip.background = list(col = 'transparent'),
                             axis.line = list(col = 'transparent')),
                           scales = list(draw = FALSE),
                           col.regions = Pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                           par.strip.text = list(cex = 0.8,
                                                 lines = 1,
                                                 col = "black"),
                           panel = function(...){
                             lattice::panel.levelplot.raster(...)
                             sp::sp.polygons(shpLoaded, fill = 'black', lwd = 1)
                           }))

dev.off()

###########################
###########################

# Make average 2011 and 2100 biomass and leading maps

source('~/projects/NWT/posthocFunctions/bioLeadMaps.R')
pathData <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS"

treeSpecies = c("Betu_Pap","Lari_Lar","Pice_Gla",
                "Pice_Mar","Pinu_Ban","Popu_Tre")

leadingPercentage <- 0.75
  
pal <- c("#27408B", "#8B7D6B", "#CD0000", "#EEB422", "#9A32CD", "#006400",
         "#A6BFFF", "#F1E3D1", "#FF7F7F", "#FFDC66", "#FFB1FF", "#7FE37F")

Require("maptools")

shpLoaded <- maptools::readShapeLines("~/projects/NWT/inputs/NWT_NT1_BCR6_2011/RSFshp.shp")

lead2011_CCSM4 <- bioLeadMaps(Year = 2011,
                        run = "run1",
                        climateScenario = "CCSM4",
                        pathData,
                        shp = shpLoaded,
                        pal = pal,
                        pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
                        leadingPercentage = 0.75)

lead2100_CCSM4 <- bioLeadMaps(Year = 2100,
                        run = "run1",
                        climateScenario = "CCSM4",
                        pathData,
                        shp = shpLoaded,
                        pal = pal,
                        pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
                        leadingPercentage = 0.75)

lead2011_CanESM2 <- bioLeadMaps(Year = 2011,
                        run = "run2",
                        climateScenario = "CanESM2",
                        pathData,
                        shp = shpLoaded,
                        pal = pal,
                        pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
                        leadingPercentage = 0.75)

lead2100_CanESM2 <- bioLeadMaps(Year = 2100,
                        run = "run2",
                        climateScenario = "CanESM2",
                        pathData,
                        shp = shpLoaded,
                        pal = pal,
                        pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
                        leadingPercentage = 0.75)

lead2011_INM_CM4 <- bioLeadMaps(Year = 2011,
                        run = "run3",
                        climateScenario = "INM-CM4",
                        pathData,
                        shp = shpLoaded,
                        pal = pal,
                        pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
                        leadingPercentage = 0.75)

lead2100_INM_CM4 <- bioLeadMaps(Year = 2100,
                        run = "run3",
                        climateScenario = "INM-CM4",
                        pathData,
                        shp = shpLoaded,
                        pal = pal,
                        pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/vegetationPlots",
                        leadingPercentage = 0.75)
