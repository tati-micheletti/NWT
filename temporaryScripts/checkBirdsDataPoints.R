source("~/projects/NWT/modules/birdsNWT/R/loadBirdModels.R")
source("~/projects/NWT/functions/getAllPredictedBirds.R")

library("Require")
Require("usefulFuns")
Require("data.table")
Require("reproducible")
Require("googledrive")

birds <- getAllPredictedBirds("outputs/landscapeRuns/LandR.CS_fS/CCSM4_run1/birdPredictionsV8")

birdModels <- loadBirdModels(birdsList = birds[["birdSpecies"]], 
                             folderUrl = "", 
                             pathData = "modules/birdsNWT/data", 
                             version = "8")

mods <- rbindlist(lapply(names(birdModels), FUN = function(sp){
  MOD <- birdModels[[sp]]
  Abundance <- MOD$gbm.call$dataframe[["ABUND"]]
  counts <- sum(Abundance != 0)
  DT <- data.table(Species = sp,
                   counts = counts)
  return(DT)
}))

mods

birdsGroupingTable <- prepInputs(url = "https://drive.google.com/file/d/1SGJ5ABhafT97wm2wIUyK6bIyWeTZ4e7S/pub?output=csv", 
                                 targetFile = "Bird Classification - birdHabitatRevised.csv",
                                 destinationPath = tempdir(), 
                                 fun = "data.table::fread", 
                                 header = TRUE)
birdsGroupingTable <- birdsGroupingTable[, c("Species Code", "Habitat")]
names(birdsGroupingTable) <- c("Species", "Habitat")

tableFull <- merge(mods, birdsGroupingTable, by = "Species")
Table1 <- file.path(getwd(), "outputs/landscapeRuns/LandR.CS_fS/hotspots/Table1.csv")
write.csv(tableFull, Table1)
drive_upload(Table1, path = as_id("1yzfYnVqAANyfPRQeTu-l8YUl2hW6FbBk"))

# NOT WHAT I NEED. NEED DIANA'S CODE!

# Get all covariate values for each bird for each model
# birdCovariatesTable <- rbindlist(lapply(birds$birdSpecies, function(BIRD){
#   # Birds CS
#   DT <- data.table(birdModels[[BIRD]][["contributions"]])
#   DT[, species := BIRD]
#   return(DT)
# }))
# 
# write.csv(birdCovariatesTable, file = file.path(getwd(), "birdCovariates.csv"))
# library("googledrive")
# drive_upload(file.path(getwd(), "birdCovariates.csv"), path = as_id("1wxQ6xtIg3A6OgVivuqCi4vbvr5uEQppd"))


DT <- data.table(birdModels[[1]][["gbm.call"]][["dataframe"]]) # It is the same dataframe for all species!
colsToKeep <- names(DT)[7:length(names(DT))]
DT <- DT[, ..colsToKeep]
otherInfo <- rbindlist(lapply(names(DT), function(variable){
  DF <- DT[, ..variable]
  DF <- DF[[1]]
  newDT <- data.table(variable = variable,
                      Min = round(min(as.numeric(DF), na.rm = TRUE), 2),
                      Max = round(max(as.numeric(DF), na.rm = TRUE), 2)
  )
}))
Table2 <- file.path(getwd(), "outputs/landscapeRuns/LandR.CS_fS/hotspots/variableMinMax.csv")
write.csv(otherInfo, file = Table2)
drive_upload(Table2, path = as_id("1yzfYnVqAANyfPRQeTu-l8YUl2hW6FbBk"))

