#########################################################
##                 H O T S P O T S                     ##
#########################################################

SpaDES.core::setPaths(cachePath = hotspotsCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       toupper(format(Sys.time(), "%d%b%y")),
                                                       definedRun$whichRUN,
                                                       replicateNumber),
                                             create = TRUE))

if (all(runLandR == FALSE)){
  if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the 
                                            analysis (i.e. where LandR results are)")
  newInputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                       replacement = originalDateAnalysis)
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  setPaths(inputPath = newInputPath,
           outputPath = newOutputPath)
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                     "hotspots"), create = TRUE)
  setPaths(outputPath = hotOutPath)
} else {
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots"), create = TRUE)
  setPaths(inputPath = Paths$outputPath,
           outputPath = hotOutPath)
}

# Set step interval
stepInterval <- 20 # What is the interval for which there is data?


# Get bird data
source('~/projects/NWT/functions/getAllPredictedBirds.R')
allBirds <- getAllPredictedBirds(pathToBirdsRasters = file.path(Paths$inputPath, 
                                                                "birdPredictionsV8"))
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)

tic("Bringing bird models to memory elapsed time: ")
birdPrediction <- lapply(yearsWanted, function(Y){
  allSp <- lapply(allBirds[["birdSpecies"]], function(sp){
    BIRDS <- raster::raster(file.path(Paths$inputPath, "birdPredictionsV8", 
                                      paste0(definedRun$whichReplicate, "_", definedRun$whichRUN,
                                   "predicted", sp, "Year", Y, ".tif")))
    currSp <- which(sp == allBirds[["birdSpecies"]])
    totSp <- length(allBirds[["birdSpecies"]])
    message(paste0("Bringing ", sp, " to memory for year ", Y, 
                   ". ", currSp, " of ", totSp," species."))
    BIRDS[] <- BIRDS[]
    return(BIRDS)
  })
  names(allSp) <- allBirds[["birdSpecies"]]
  return(allSp)
})
names(birdPrediction) <- paste0("Year", yearsWanted)
toc()

tic("Bringing caribou models to memory elapsed time: ")
predictedPresenceProbability <- lapply(yearsWanted, function(Y){
  bothLays <- lapply(c("relativeSelectioncaribouRSF_NT_Year",
                       "relativeSelectionUncertaincaribouRSF_NT_Year"), function(nameStr){
    Lay <- raster::raster(file.path(Paths$inputPath, "caribouPredictions", 
                                      paste0(nameStr, Y, ".tif")))
    message(paste0("Bringing caribou ", nameStr, 
                   " to memory for year ", Y))
    Lay <- binRSFtoDeMars2019(Lay)
    Lay[] <- Lay[]
    return(Lay)
  })
  names(bothLays) <- c("rasterOfAverage",
                       "rasterOfUncertain")
  return(bothLays)
})
names(predictedPresenceProbability) <- paste0("Year", yearsWanted)
toc()

# Locking anthropogenic disturbances out of solutions
anthropoDisturbances <- dropLayer(x = anthropogenicLayers, i = 1)
anthropoDisturbances <- calc(anthropoDisturbances, fun = sum, na.rm = TRUE)
anthropoDisturbances[is.na(rasterToMatch[])] <- NA
anthropoDisturbances[anthropoDisturbances[] > 2.99] <- NA
anthropoDisturbances[!is.na(anthropoDisturbances)] <- 1

# Removed "anthropogenicLayer_PP" = objects$anthropogenicLayers[["lden1000_2015"]] because it is
# not what I expected.  anthropogenicLayer_PP needs to be a layer with the unbuffered anthropogenic
# disturbances
modules <- list("priorityPlaces_DataPrep", 
                "priorityPlaces")

if (!exists("Times"))
  Times <- list(start = 2031, 
              end = 2071)

# For the hotspot analysis, we need 3 layers for coarse filter: 
#   1. Physiographic units: Level IV Ecoregion (ecoRegionRAS)
#   2. Landscape Units: soil-based layer (landscapeUnitsRAS)
#   3. Vegetation types: Land Cover Class layer (rstLCC)
# Both 2 and 3 should be further stratified using 1.

# 1. Get them in tables
coarseTable <- data.table(landscapeUnits = getValues(landscapeUnitsRAS),
                          vegetationTypes = getValues(rstLCC),
                          physiographicUnits = getValues(ecoRegionRAS))
# 2. Create new categories for both vegetationTypes and landscapeUnits
coarseTable[!is.na(physiographicUnits), c("strVegetationTypes", "strLandscapeUnits") := list(paste(vegetationTypes, 
                                                                         physiographicUnits,
                                                                         sep = "_"),
                                                                   paste(landscapeUnits, 
                                                                         physiographicUnits,sep = "_"))]
landscapeUnitsStr <- setValues(x = raster(landscapeUnitsRAS), 
                               values = as.factor(coarseTable[["strLandscapeUnits"]]))

vegetationTypesStr <- setValues(x = raster(rstLCC), 
                               values = as.factor(coarseTable[["strVegetationTypes"]]))

coarseFilterStack <- raster::stack(ecoRegionRAS, vegetationTypesStr, landscapeUnitsStr)
names(coarseFilterStack) <- c("physiographicUnits", "vegetationTypes", "landscapeUnits")

# Make coarse filter features
coarseFilterFeatures <- makeCoarseFilterStack(stk = coarseFilterStack,
                                              years = yearsWanted, 
                                              maxSizeForVerySmall = list("physiographicUnits" = 10000,
                                                                         "vegetationTypes" = 10000,
                                                                         "landscapeUnits" = 1000))
objects <- c(objects, list("birdPrediction" = birdPrediction,
                             "predictedPresenceProbability" = predictedPresenceProbability))

planningUnit <- makePlanningUnit(years = yearsWanted,
                                 pU = rasterToMatch)
penaltyValue <- 100*calculatePenalty(planningUnit = planningUnit[[1]])

objects <- c(objects, list("planningUnit" = planningUnit,
                           "otherFeatures" = coarseFilterFeatures,
                           "protectedAreas" = protectedAreas))

allExp <- c("XIII", "XIV")

for (EXP in allExp) {
  experimentName <- EXP # And II, III ... XIV

if (!exists("experimentName"))
  experimentName <- "I"

targsNremovs <- definePPtargetsAndLayers(experimentName = experimentName)

# Sys.sleep(20*60)
parameters <- list(
  "priorityPlaces_DataPrep" = list(
    diversityIndex = "shannon",
    typeOfAnalysis = "standard",
    stepInterval = stepInterval,
    normalizeRasters = FALSE,
    cleanFeatures = TRUE,
    removeLayers = targsNremovs$removeLayers
  ),
  "priorityPlaces" = list(
    stepInterval = 20,
    binaryDecision = FALSE,
    penalty = penaltyValue, # penalty = 300, edge_factor = 0.5
    verbose = TRUE,
    threads = 6,
    solver = "gurobi",
    firstFeasible = FALSE,
    gap = 0.01,
    experimentName = experimentName,
    targets = targsNremovs$targets
    # Can be one value or a vector of each corresponding to one feature
    # constraintType = list(add_locked_out_constraints = anthropoDisturbances)
    # "constraintType" = raster of protected areas in NWT
  )
)

trackSeed(replic = definedRun$whichReplicate, runName = runName)
assign(
  x = paste0("PP_", experimentName),
  do.call(
    get(spadesFun),
    args = alist(
      times = Times,
      params = parameters,
      modules = modules,
      objects = objects,
      paths = Paths,
      loadOrder = unlist(modules)))) #Add outputs at some point
}
# Only one at a time
# diversityIndex = "simpson"
# Other weights



