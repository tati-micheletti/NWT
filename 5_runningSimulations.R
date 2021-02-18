#########################################################
##              S I M U L A T I O N S                  ##
#########################################################

SpaDES.core::setPaths(cachePath = simulationsCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       toupper(format(Sys.time(), "%d%b%y")),
                                                       definedRun$whichRUN,
                                                       replicateNumber),
                                             create = TRUE))
if (isTest)
  SpaDES.core::setPaths(outputPath = gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")),
                                          replacement = "Tests"))
Paths

originalInputsPath <- Paths$inputPath

if (!exists("runLandR")) runLandR <- FALSE # Default if not provided
if (runLandR){
  message(crayon::red(paste0("Starting ", ifelse(runOnlySimInit, "simInit", "simulations"), 
                             " for ", definedRun$whichRUN, " ", 
                             definedRun$whichReplicate, " for ", runName)))
  t1 <- Sys.time()
  if (!exists("Inputs"))
    Inputs <- data.frame()
  parameters[["fireSense_dataPrep"]][["skipMDCprep"]] <- TRUE
  trackSeed(replic = definedRun$whichReplicate, runName = runName)
  assign(x = definedRun$whichRUN, do.call(get(spadesFun), args = alist(inputs = Inputs, 
                                                                      times = Times,
                                                                      params = parameters,
                                                                      modules = definedRun$modules,
                                                                      objects = objects,
                                                                      paths = Paths,
                                                                      loadOrder = unlist(definedRun$modules),
                                                                      debug = list(file = list(
                                                                        file = file.path(Paths$outputPath, 
                                                                                         "sim.log"),
                                                                                       append = TRUE), 
                                                                        debug = 1),
                                                                      outputs = outputsLandR)))
  t2 <- Sys.time()
  message(crayon::green(paste0("Finished ", ifelse(runOnlySimInit, "simInit", "simulations")," for ", 
                               definedRun$whichRUN, ". Elapsed time: ", t2-t1)))
} # End runLandR

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS MODEL 1

if (!exists("runBirds")) runBirds <- FALSE # Default if not provided
if (runBirds){
  tryCatch(rm(definedRun$whichRUN), error = function(e) warning("LandR run not found to remove"))
  source("functions/birdPredictionCoresCalc.R")
  if (!exists("birdModelVersion")) birdModelVersion <- c("4", "6a") # Default if not provided
  predictionInterval <- 20
  message(crayon::yellow(paste0("Starting simulations for BIRDS using ", definedRun$whichRUN, " ", 
                                definedRun$whichReplicate, " for ", runName)))
  
  bMod <- ifelse(length(birdModelVersion) == 1, birdModelVersion, birdModelVersion[1])
  
  if (!exists("hostIp")) stop("hostIp needs to be specified!") # Default if not provided
  hostTable <- data.table::data.table(ipEnd = c(97, 189, 213, 220, 58, 68),
                          availableCores = rep(52, times = 6),
                          availableRAM = c(rep(470, times = 5), 920))

  cores <- birdPredictionCoresCalc(birdSpecies = c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
                                                   "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
                                                   "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
                                                   "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
                                                   "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
                                                   "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
                                                   "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
                                                   "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA"
                                                   ),
                                   ipEnd = hostIp,
                                   availableCores = hostTable[hostIp == ipEnd, availableCores],
                                   availableRAM = hostTable[hostIp == ipEnd, availableRAM],
                                   sizeGbEachProcess = ifelse(bMod == 4, 5, 7),
                                   localHostEndIp = hostIp)
  parameters <- list(
    birdsNWT = list(
      "predictLastYear" = TRUE,
      "lowMem" = TRUE,
      "scenario" = paste(replicateNumber, vegetation, fire, sep = "_"),
      "useStaticPredictionsForNonForest" = TRUE,
      "useOnlyUplandsForPrediction" = TRUE,
      "baseLayer" = 2005,
      "overwritePredictions" = FALSE,
      "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from 
                                      # LandR_Biomass simulations to run it with
      "predictionInterval" = predictionInterval,
      "nCores" = "auto", # If not to parallelize, use 1
      "version" = bMod, # VERSION 6 of the modules has both climate and vegetation as covariates for the model
      "RCP" = RCP,
      "climateModel" = climateModelType,
      "ensemble" = ensemble,
      "climateResolution" = climateResolution,
      "climateFilePath" = climateFilePath
    )
  )
  
  modules <- list("birdsNWT")
  
  invisible(sapply(X = list.files(file.path(Paths$modulePath, "birdsNWT/R/"), 
                                  full.names = TRUE), FUN = source))
  if (all(runLandR == FALSE)){
    if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the 
                                            analysis (i.e. where LandR results are)")
    newInputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                         replacement = originalDateAnalysis)
    newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                          replacement = originalDateAnalysis)
    setPaths(inputPath = newInputPath,
             outputPath = newOutputPath)
    birdOutPath <- checkPath(file.path(newOutputPath, 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), create = TRUE)
    setPaths(outputPath = birdOutPath)
  } else {
    birdOutPath <- checkPath(file.path(Paths$outputPath, 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), create = TRUE)
    setPaths(inputPath = Paths$outputPath,
             outputPath = birdOutPath)
  }
  
  tryCatch({
    pixelsWithDataAtInitialization <- readRDS(file.path(Paths$inputPath,
                                                        "activePixelIndex_year2011.rds"))
  },
  error = function(e){
    stop(
      paste0(
        "The activePixelIndex_year2011.rds object was not found. Is the inputs folder set correctly?",
        "Current inputs folder: ", Paths$inputPath,
        " This affects bird predictions for the NWT (i.e. density will not be predicted",
        "for pixels were total biomas = 0). To fix this, save the object named", 
        "'sim$activePixelIndex' in the end of Biomass_core's init i.e. add to Biomass_core's ",
        "Line 661-662: 
saveRDS(sim$activePixelIndex, file = file.path(outputPath(sim), 'pixelsWithDataAtInitialization.rds'))"
      )
    )
  })
  
  objects <- c(objects, list(
    "uplandsRaster" = uplandsRaster,
    "climateDataFolder" = file.path(originalInputsPath, climateModel, "CCSM4_RCP85_annual"), # <~~~~~~~~~~~~ FIX THIS FOR BIRDS!
    "pixelsWithDataAtInitialization" = pixelsWithDataAtInitialization
  ))
  for (GROUP in 1:length(cores$birdSpecies)) {
    objects <- c(objects, list("birdsList" = cores$birdSpecies[[GROUP]]))
    simulation <- paste0(definedRun$whichRUN, "_birdsV",
                         parameters[["birdsNWT"]][["version"]])
    trackSeed(replic = definedRun$whichReplicate, runName = runName)
  assign(
    x = simulation,
    do.call(
      get(spadesFun),
      args = alist(
        times = Times,
        params = parameters,
        modules = modules,
        objects = objects,
        paths = Paths,
        loadOrder = unlist(modules),
        outputs = outputsLandR)))
  } # ENd for-loop GROUPS
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS MODEL 2
  
  if (length(birdModelVersion) > 1){ # Run a second bird model
    tryCatch(rm(definedRun$whichRUN), error = function(e) warning("LandR run not found to remove"))
    bMod <- birdModelVersion[2]
    parameters[["birdsNWT"]][["version"]] <- bMod
    birdOutPath <- checkPath(file.path(dirname(Paths$outputPath), 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), 
                             create = TRUE)
    setPaths(outputPath = birdOutPath)
    
    cores <- birdPredictionCoresCalc(birdSpecies = c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
                                                     "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
                                                     "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
                                                     "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
                                                     "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
                                                     "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
                                                     "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
                                                     "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA"
                                                     ),
                                     ipEnd = hostIp,
                                     availableCores = hostTable[hostIp == ipEnd, availableCores],
                                     availableRAM = hostTable[hostIp == ipEnd, availableRAM],
                                     sizeGbEachProcess = ifelse(bMod == 4, 5, 7),
                                     localHostEndIp = hostIp)

    for (GROUP in 1:length(cores$birdSpecies)) {
      objects <- c(objects, list("birdsList" = cores$birdSpecies[[GROUP]]))
      simulation <- paste0(definedRun$whichRUN, "_birdsV",
                           parameters[["birdsNWT"]][["version"]])
      trackSeed(replic = definedRun$whichReplicate, runName = runName)
      assign(
        x = simulation,
        do.call(get(spadesFun),
               args = alist(
                 times = Times,
                 params = parameters,
                 modules = modules,
                 objects = objects,
                 paths = Paths,
                 loadOrder = unlist(modules),
                 outputs = outputsLandR
               )
             ))
    } # ENd for-loop GROUPS
  } # End of second bird model
} # End of run birds

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CARIBOU

if (!exists("runCaribou")) runCaribou <- FALSE # Default if not provided
if (runCaribou){
  tryCatch(rm(definedRun$whichRUN), error = function(e) warning("LandR run not found to remove"))
  message(crayon::white(paste0("Starting simulations for CARIBOUS using ", definedRun$whichRUN, " ", 
                               definedRun$whichReplicate, " for ", runName)))
  if (all(runLandR == FALSE, runBirds == FALSE)){
    if (is.null(originalDateAnalysis)) 
      stop("If runLandR == FALSE you need to pass the date for the
           analysis (i.e. where LandR results are)")
    newInputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                         replacement = originalDateAnalysis)
    newOutputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                          replacement = originalDateAnalysis)
    setPaths(inputPath = newInputPath,
             outputPath = file.path(newOutputPath, "caribouPredictions"))
  } else {
    if (runBirds == TRUE){ # input path is correct, independently if I ran LandR before birds
      caribouOutPath <- checkPath(file.path(Paths$outputPath, "caribouPredictions"), create = TRUE)
      setPaths(outputPath = caribouOutPath)
    } else { # only if I didn't run birds, only LandR
      caribouOutPath <- checkPath(file.path(Paths$outputPath, "caribouPredictions"), create = TRUE)
      setPaths(inputPath = Paths$outputPath,
               outputPath = caribouOutPath)
    }
  }
  
  invisible(sapply(X = list.files(file.path(Paths$modulePath, "caribouRSF_NT/R/"), 
                                  full.names = TRUE), FUN = source))
  parameters <- list(
    caribouRSF_NT = list(
      "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
      "predictionInterval" = 10,
      "simulationProcess" = "dynamic",
      plotTime = NA,
      cropRSFToShp = FALSE,
      makeAssertions = FALSE
    ),
    caribouPopGrowthModel = list(
      ".plotInitialTime" = NULL,
      "recoveryTime" = 40,
      ".useDummyData" = FALSE,
      ".growthInterval" = 10,
      "recruitmentModelVersion" = "Johnson", # Johnson or ECCC
      "recruitmentModelNumber" = "M4",
      "femaleSurvivalModelNumber" = c("M1", "M4") # M1:M5 --> best models: M1, M4
    ) 
    # ATTENTION: recruitmentModelNumber and recruitmentModelVersion need to be paired. ie.
    # if you want to run M3 from ECCC and M1 and M4 from Johnson you should put these as
    #     "recruitmentModelVersion" = c("ECCC", "Johnson", "Johnson"),
    #     "recruitmentModelNumber" = c("M3", "M1", "M4"), 
    # otherwise it will repeat the recruitmentModelVersion for all recruitmentModelNumber
  )
  modules <- list("caribouRSF_NT", "caribouPopGrowthModel")
  simulationBoo <- paste0(definedRun$whichRUN, "_caribou")
  trackSeed(replic = definedRun$whichReplicate, runName = runName)
  rstCurrentBurnList <- readRDS(file.path(Paths$inputPath, 
                                          "rstCurrentBurnList_year2100.rds"))
  objects <- c(objects, list("rstCurrentBurnList" = rstCurrentBurnList,
                             "runName" = runName))
  assign(x = simulationBoo,
         do.call(
           get(spadesFun),
           args = alist(
             times = Times,
             params = parameters,
             modules = modules,
             objects = objects,
             paths = Paths,
             loadOrder = unlist(modules),
             outputs = outputsLandR,
             debug = 1
           )
         ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

