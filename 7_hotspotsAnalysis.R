#########################################################
##                 H O T S P O T S                     ##
#########################################################

SpaDES.core::setPaths(cachePath = hotspotsCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       toupper(format(Sys.time(), "%d%b%y")),
                                                       definedRun$whichRUN,
                                                       replicateNumber),
                                             create = TRUE))
generalOutputs <- dirname(file.path(getwd(), "outputs",
                                    "landscapeRuns",
                                    definedRun$whichRUN,
                                    replicateNumber))
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
                                     "hotspots/ms1"), create = TRUE)
  setPaths(outputPath = hotOutPath)
} else {
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots_ms1"), create = TRUE)
  setPaths(inputPath = newOutputPath,
           outputPath = hotOutPath)
}

if (!exists("Times"))
  Times <- list(start = 2011, 
                end = 2100)

# Set step interval
stepInterval <- 20 # What is the interval for which there is data?
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)

# Get bird data
if (!exists("predictWithBirds"))
  predictWithBirds <- FALSE

if (predictWithBirds){
  source('~/projects/NWT/functions/getAllPredictedBirds.R')
  allBirds <- getAllPredictedBirds(pathToBirdsRasters = file.path(Paths$inputPath, 
                                                                  "birdPredictionsV8"))
if (!exists("MakeAnalysisForBirdGroup"))
    MakeAnalysisForBirdGroup <- FALSE
  
if (MakeAnalysisForBirdGroup){
  birdsGroupingTable <- prepInputs(url = "https://drive.google.com/file/d/1SGJ5ABhafT97wm2wIUyK6bIyWeTZ4e7S/pub?output=csv", 
                                   targetFile = "Bird Classification - birdHabitatRevised.csv",
                                   destinationPath = generalOutputs, 
                                   fun = "data.table::fread", 
                                   header = TRUE)
# Assertion
  if (!"mixedwood" %in% unique(birdsGroupingTable[["Habitat"]]))
    stop("Attention: New bird table not being used!!")
    
  # Simplyfying and putting the correct names
  birdsGroupingTable <- birdsGroupingTable[, c("Species Code", "Habitat")]
  names(birdsGroupingTable) <- c("species", "habitat")
  
  if (!exists("whichGroup"))
    whichGroup <- "conifer"
# "shrub", "generalist", "deciduous", "conifer", "wetland", "grassland", "mixedwood" 
  allBirds[["birdSpecies"]] <- birdsGroupingTable[habitat == whichGroup, species]
}
} else {
  tic("No birds for this scenario elapsed time: ")
  allBirds <- NULL 
  birdPrediction <- NA 
  toc()
}

# Locking anthropogenic disturbances out of solutions
anthropoDisturbances <- dropLayer(x = anthropogenicLayers, i = 1)
anthropoDisturbances <- raster::calc(anthropoDisturbances, fun = sum, na.rm = TRUE)
anthropoDisturbances[is.na(rasterToMatch[])] <- NA
anthropoDisturbances[anthropoDisturbances[] > 2.99] <- NA
anthropoDisturbances[!is.na(anthropoDisturbances)] <- 1

# Removed "anthropogenicLayer_PP" = objects$anthropogenicLayers[["lden1000_2015"]] because it is
# not what I expected.  anthropogenicLayer_PP needs to be a layer with the unbuffered anthropogenic
# disturbances
modules <- list("priorityPlaces_DataPrep", 
                "priorityPlaces")

  # For the hotspot analysis with coarse filters, we need 3 layers for coarse filter: 
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
planningUnit <- makePlanningUnit(years = yearsWanted,
                                 pU = rasterToMatch)

objects <- c(objects, list("planningUnit" = planningUnit,
                           "otherFeatures" = coarseFilterFeatures,
                           "protectedAreas" = protectedAreas))

# allExp <- as.character(as.roman(32:41))
# allExp <- as.character(as.roman(40))
allExp <- as.character(as.roman(41))

allTimes <- seq(Times[["start"]], Times[["end"]], by = 20)
message(paste0("Running simulations for years ", paste(allTimes, collapse = ", ")))
tic(paste0("Total time elapsed for all ", length(allExp)," scenarios, with ", 
           length(allTimes)," point in time: "))
for (EXP in allExp) {
 experimentName <- EXP # And II, III ... XLI;
 
 # NEW:
    # 22:31 --> Prioritizing for caribou
    # 32:41 --> Prioritizing for birds (scenario_group)
    # 42:51 --> Prioritizing for random
    
 # OLD:
    # NIL: All coarse filter layers + all individual layers where we use % of conservation based on how each species is classified
    # NIL_CFexcl: NO coarse filter layers + all individual layers where we use % of conservation based on how each species is classified
    # Current values: "Streams" 3, 4 and 5, respectively: 0.6, 0.4, 0.2
    
if (!exists("experimentName"))
  experimentName <- "XXX"

source("~/projects/NWT/functions/groupSAR.R")
source("~/projects/NWT/functions/definePPtargetsAndLayersII.R")
source("~/projects/NWT/functions/definePPtargetsAndLayersIII.R")
if (MakeAnalysisForBirdGroup) {
  speciesClass <- list(SAR = allBirds[["birdSpecies"]])
  # This does not mean all species are SAR, it is just a shortcut
  # to get all species assigned to the remaining stream (stream2) during the dataPrep
  # stage
} else {
  speciesClass <- groupSAR(speciesList = allBirds[["birdSpecies"]])
}

targsNremovs <- definePPtargetsAndLayersIII(experimentName = experimentName,
                                            planningUnit = planningUnit[[1]],
                                            withoutPenalties = FALSE,
                                            noBirds = ifelse(MakeAnalysisForBirdGroup, FALSE, TRUE))

objects$speciesClassification <- speciesClass

parameters <- list(
  "priorityPlaces_DataPrep" = list(
    diversityIndex = 'none',#"shannon", # Create the diversity index as 'none' to process all layers
    typeOfAnalysis = "standard",
    predictionYears = c(2011, 2031, 2051, 2071),
    stepInterval = stepInterval,
    normalizeRasters = FALSE,
    cleanFeatures = TRUE,
    removeLayers = targsNremovs$removeLayers,
    collapseBirds = TRUE
  ),
  "priorityPlaces" = list(
    stepInterval = 20,
    binaryDecision = FALSE,
    penalty = targsNremovs$penalty,
    verbose = TRUE,
    timeLimit = 3600,
    solutionObjective = "optimizeForArea",
    areaToConserve = targsNremovs$areaToConserve,
    threads = 11,
    # constraintType = list(add_locked_in_constraints = protectedAreas,
    #                       add_feature_contiguity_constraints = NA), # add_contiguity_constraints = NA --> No go. No solution!
    solver = "gurobi",
    firstFeasible = TRUE,
    gap = 0.01,
    experimentName = paste0(experimentName, ifelse(MakeAnalysisForBirdGroup, 
                                                   paste0("_", whichGroup), NULL)),
    targets = targsNremovs$targets # This is where we put the targets for all species
    # Can be one value or a vector of each corresponding to one feature
    # constraintType = list(add_locked_out_constraints = anthropoDisturbances)
    # "constraintType" = raster of protected areas in NWT
  )
)

if (!all(parameters[["priorityPlaces_DataPrep"]][["predictionYears"]] == unique(unlist(Times)))){
  message(crayon::red("The parameter 'predictionYears' of priorityPlaces_DataPrep is different from ",
                      "the Times parameter. Your simulation will likely fail. Please make sure that ",
                      "you pass the correct parameters in both cases. For now, Times will be overwritten",
                      " to match the 'predictionYears'"))
  pY <- parameters[["priorityPlaces_DataPrep"]][["predictionYears"]]
  Times <- list(start = pY[1], end = pY[length(pY)])
}

message(crayon::red("Running scenario ", experimentName, " for ", whichGroup))

trackSeed(replic = definedRun$whichReplicate, runName = runName)

# Check if the solution exists. If so, don't do it again, otherwise, do it.
solutionPath <- file.path(Paths$outputPath, paste0(experimentName, "_", whichGroup, 
                                                   "_solutions_Year", 
                                                   parameters[["priorityPlaces_DataPrep"]][["predictionYears"]], 
                                                   ".tif"))
allExist <- file.exists(solutionPath)

if (all(allExist)){
  message(crayon::green(paste0("All years for ", experimentName, " ", 
                               whichGroup, " exist. Moving on...")))
} else {
  
  if (predictWithBirds){
  tic("Bringing bird models to memory elapsed time: ")
  birdPrediction <- lapply(yearsWanted, function(Y){
    allSp <- lapply(allBirds[["birdSpecies"]], function(sp){
      BIRDS <- raster::raster(file.path(Paths$inputPath, "birdPredictionsV8", 
                                        paste0(definedRun$whichReplicate, "_", definedRun$whichRUN,
                                               "predicted", sp, "Year", Y, ".tif")))
      currSp <- which(sp == allBirds[["birdSpecies"]])
      totSp <- length(allBirds[["birdSpecies"]])
      message(paste0("Bringing ", sp, " to memory for year ", Y, 
                     ". ", currSp, " of ", totSp," species. ", 
                     ifelse(exists("whichGroup"),paste0("Grouping ID: ", whichGroup), NULL)))
      BIRDS[] <- BIRDS[]
      return(BIRDS)
    })
    names(allSp) <- allBirds[["birdSpecies"]]
    return(allSp)
  })
  names(birdPrediction) <- paste0("Year", yearsWanted)
  toc()
  } else {
    birdPrediction <- NA
  }

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
  
  objects <- c(objects, list("birdPrediction" = birdPrediction,
                             "predictedPresenceProbability" = predictedPresenceProbability))
  
  if (targsNremovs[["noBirds"]]){
    objects$birdPrediction <- NA
  }
  
  # Get the ones that don't exist and run them
  yearsToRun <- usefulFuns::substrBoth(tools::file_path_sans_ext(solutionPath[!allExist]), 
                                       fromEnd = TRUE, howManyCharacters = 4)
  # Reset Times
  Times <- list(start = min(as.numeric(yearsToRun)), 
                end = max(as.numeric(yearsToRun)))
  # Reset Parameters
  parameters[["priorityPlaces_DataPrep"]][["predictionYears"]] <- as.numeric(yearsToRun)

  message(crayon::red(paste0("Running years ", paste(yearsToRun, collapse = ", "),
                             " for ", experimentName, " ", 
                               whichGroup, ".")))
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
}
toc()


