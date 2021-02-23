#########################################################
##          P L O T S    C A R I B O U                 ##
#########################################################

# RUN SOURCE UNTIL script 2 

resultsFolder <- file.path(getwd(), "outputs/landscapeRuns/LandR.CS_fS")

SpaDES.core::setPaths(cachePath = posthocCache,
                      outputPath = checkPath(file.path(resultsFolder, 
                                                       "caribouPlots"),
                                             create = TRUE))
source('posthocFunctions/makeCaribouRSFAverageMap.R')
source('posthocFunctions/convertShpToRas.R')
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

booRSF <- makeCaribouRSFAverageMap(resultsFolder = resultsFolder,
                                   runs = paste0("run", 1:5), 
                                   climateModels = c("CCSM4", "CanESM2", "INM-CM4"), 
                                   outputsPath = Paths$outputPath,
                                   shp = booSHP,
                                   binningTable = binningTable,
                                   initialYear = 2011,
                                   lastYear = 2100)

# MAKE AVERAGE PER YEAR FOR EACH CLIMATE SCENARIO -- time series of average RSF?
# HERE <~~~~~~~~~~~~~~~~~~~~~~~~~~ Average per year per climate scenario
# REDO SDs!!
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
                                      whichPolys = c("Dehcho South_v2", "Dehcho North_v2", 
                                                     "Hay River Lowlands", "GSA South", "GSA North"),
                                      outputFolder = pathOutputs)

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
pl <- biomassPlotsCaribou(years = c(2011, 2100),
                          pathData = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                          pathOutputs = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                          Scenarios = c("CCSM4",
                                        "CanESM2",
                                        "INM-CM4"),
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
library(ggplot2)
library(gridExtra)
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
