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
                                   climateModels = c("CCSM4", "CanESM2"), 
                                   outputsPath = Paths$outputPath,
                                   shp = booSHP,
                                   binningTable = binningTable,
                                   initialYear = 2011,
                                   lastYear = 2100)


# To be worked on...
source('~/projects/NWT/modules/caribouPopGrowthModel/R/plotCaribouPopGrowth.R')
plotCaribou <- plotCaribouPopGrowth(startTime = 2011,
                                    currentTime = 2100,
                                    endTime = 2100,
                                    resultsMainFolder = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                                    yearSimulationStarts = 2011,
                                    outputFolder = pathOutputs)

# Digest disturbance
allDist <- list.files(resultsFolder, pattern = "disturbances_year2041", 
                      full.names = TRUE, recursive = TRUE)
allcombs <- data.table(expand.grid(c("CCSM4_"), paste0("run", 1)))
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
qs::qsave(x = distTable, file = file.path(Paths$outputPath, "disturbanceTable_Fixed.qs"))
Require("googledrive")
drive_upload(file.path(Paths$outputPath, "disturbanceTable_Fixed.qs"), 
             path = as_id("1-UqiY7_iEFyrEtRHKDcDfbkSGvc7CR5I"))

# Require("qs")
# Require("googledrive")
# caribouDisturbance <- prepInputs(url = paste0("https://drive.google.com/file/d/",
#                                               "1dhyqzAPsMWKlC04y11bImVeAx4o7KvW9/view?usp=sharing"),
#                                  targetFile = "disturbanceTable_Fixed.qs",
#                                  destinationPath = tempdir(), fun = "qs::qread")
