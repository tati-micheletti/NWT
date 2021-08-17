# CHECKING THE DATA FROM SIMULATION

caribous <- LandR.CS_fS_caribou

disturbanceTable <- rbindlist(lapply(names(caribous$disturbances), function(YYYY){
  DTshape <- rbindlist(lapply(names(caribous$disturbances[[YYYY]]), function(shape){
    DTpoly <- rbindlist(lapply(names(caribous$disturbances[[YYYY]][[shape]]), function(Herd){
      DT <- data.table(caribous$disturbances[[YYYY]][[shape]][[Herd]])
      DT[, c("Year", "Shapefile", "Herd") := list(as.numeric(strsplit(YYYY, "Year")[[1]][2]),
                                                  shape, Herd)]
      return(DT)
    }))
    return(DTpoly)
  }))
  return(DTshape)
}))

disturbance <- disturbanceTable[, c("fire_excl_anthro", "Anthro", "Herd", "Year")]
disturbance[Herd == "Dehcho North_v2", Herd := "Dehcho North"]
disturbance[Herd == "Dehcho South_v2", Herd := "Dehcho South"]

pred <- caribous$predictedCaribou

predictedTable <- rbindlist(lapply(names(pred), function(YYYY){
  DT <- pred[[YYYY]]
  DT[, Year := as.numeric(strsplit(YYYY, "Year")[[1]][2])]
  return(DT)
}))

predictions <- predictedTable[, c("annualLambda","polygon", "Year")]
names(predictions)[names(predictions) == "polygon"] <- "Herd"
predictions[Herd == "Dehcho North_v2", Herd := "Dehcho North"]
predictions[Herd == "Dehcho South_v2", Herd := "Dehcho South"]

combinedTable <- merge(disturbance, predictions, by = c("Herd", "Year"))
PolsOfInterest <- c("Dehcho North", "Dehcho South", "GSA North", "GSA South", "Hay River Lowlands")

combinedTable[Herd %in% PolsOfInterest &
                Year == 2011, ]

# AFTER SIMULATION
library("Require")
Require("reproducible")
Require("googledrive")

pathOutputs <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/"
resultsFolder <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/"
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

disturbance <- distTable[!Polygon %in% c("Bistcho", "Maxhamish", "Yates") &
                           Area == "metaHeards",
                         c("fire_excl_anthro", "Anthro", "Polygon", "Year", "run", "Model")] 

names(disturbance)[names(disturbance) == "Polygon"] <- "Herd"
names(disturbance)[names(disturbance) == "Model"] <- "climateModel"

fl <- list.files(path = resultsFolder, pattern = "predictedCaribou_year2100.rds", 
                 full.names = TRUE, recursive = TRUE)
lambs <- rbindlist(lapply(fl, function(flName){
  TB <- readRDS(flName)
  RUN <- usefulFuns::substrBoth(strsplit(x = flName, split = "run")[[1]][2], 
                                howManyCharacters = 1, 
                                fromEnd = FALSE)
  clim <- ifelse(grepl(pattern = "CanESM2", x = flName), "CanESM2",
                 ifelse(grepl(pattern = "CCSM4", x = flName), "CCSM4", "INM-CM4"))
  predictedTable <- rbindlist(lapply(names(TB), function(YYYY){
    DT <- TB[[YYYY]]
    # DT[, Year := as.numeric(strsplit(YYYY, "Year")[[1]][2])] # Was already present...
    DT[, run := paste0("run", RUN)]
    DT[, climateModel := clim]
    DT <- DT[!Herd %in% c("Bistcho", "Maxhamish", "Yates") &
               area == "metaHeards",]
    return(DT)
  }))
  return(predictedTable)
}))

combinedTable <- merge(lambs, disturbance, by = c("Herd", "Year", "run", "climateModel"))

combinedTable <- unique(combinedTable)

DT <- unique(combinedTable[, c("Herd", "Year", "climateModel", "run", 
                               "femSurvMod_recrMod", "fire_excl_anthro", "Anthro",
                               "annualLambda", "annualLambdaMax", "annualLambdaMin")])
write.csv(DT, file = file.path(pathOutputs, "disturbanceLambdaTable.csv"))
drive_upload(file.path(pathOutputs, "disturbanceLambdaTable.csv"), as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))

write.csv(combinedTable, file = file.path(pathOutputs, "fullDisturbanceLambdaTable.csv"))
drive_upload(file.path(pathOutputs, "fullDisturbanceLambdaTable.csv"), as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))

simpDT <- Copy(DT)

simpDT[, meanLambda := mean(annualLambda), by = c("Herd", "Year", "femSurvMod_recrMod")]
simpDT[, maxLambda := max(annualLambdaMax), by = c("Herd", "Year", "femSurvMod_recrMod")]
simpDT[, minLambda := min(annualLambdaMin), by = c("Herd", "Year", "femSurvMod_recrMod")]
simpDT[, meanFireExAnthro := mean(fire_excl_anthro), by = c("Herd", "Year", "femSurvMod_recrMod")]
simpDT[, minFireExAnthro := min(fire_excl_anthro), by = c("Herd", "Year", "femSurvMod_recrMod")]
simpDT[, maxFireExAnthro := max(fire_excl_anthro), by = c("Herd", "Year", "femSurvMod_recrMod")]

sDT <- unique(simpDT[, c("Herd", "Year", "femSurvMod_recrMod", 
                         "meanFireExAnthro", "minFireExAnthro", "maxFireExAnthro", 
                         "Anthro","meanLambda", "maxLambda", "minLambda")])

write.csv(sDT, file = file.path(pathOutputs, "disturbanceLambdaTable_unified.csv"))
drive_upload(file.path(pathOutputs, "disturbanceLambdaTable_unified.csv"), as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))
