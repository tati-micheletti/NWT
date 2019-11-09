# Before running this script, read !sourceScript to know the 4 
# parameters that needed to define the run 

# googledrive::drive_auth(use_oob = TRUE) # USE ONLY ONCE, the first time you are running the project 
# USING RStudio Server.

usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
googledrive::drive_auth(email = usrEmail)

if (pemisc::user() %in% c("Tati", "tmichele"))
  setwd("/mnt/data/Micheletti/NWT")
t1 <- Sys.time()
updateCRAN <- FALSE
updateGithubPackages <- FALSE
updateSubmodules <- FALSE

if (updateCRAN)
  update.packages(checkBuilt = TRUE)

if (updateGithubPackages){
  devtools::install_github("PredictiveEcology/reproducible@development")
  devtools::install_github("tati-micheletti/usefun") # Updates LandR
  devtools::install_github("achubaty/amc@development")
  devtools::install_github("PredictiveEcology/pemisc@development")
  devtools::install_github("PredictiveEcology/map@development")
  devtools::install_github("PredictiveEcology/LandR@development") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("ianmseddy/LandR.CS@master") # Climate sensitivity in LandR
}

if (updateSubmodules){
  system(paste0("cd ", getwd(),
                " && git submodule foreach git pull"), wait = TRUE)
  system(paste0("cd ", getwd(),
                " && git pull"), wait = TRUE)
  system("git submodule", wait = TRUE) # checks if the branches and commits you are using are the correct ones
} 

library("usefun")
library("LandR")
library("LandR.CS")
library("SpaDES")
library("raster")
library("plyr"); library("dplyr")
library("amc")
library("magrittr") # for piping
library("future")
library("future.apply")
library("future.callr")


# Source all common functions
# invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))
source("/mnt/data/Micheletti/NWT/functions/not_included/pathsSetup.R")
source("/mnt/data/Micheletti/NWT/functions/defineRun.R")

if (!exists("vegetation")) vegetation <- "LandR" # Default if not provided
if (!exists("fire")) fire <- "SCFM" # Default if not provided
if (!exists("replicateNumber")) replicateNumber <- NULL # Default if not provided

definedRun <- defineRun(replicateNumber = replicateNumber, 
                        vegetation = vegetation, 
                        fire = fire)
# vegetation = "LandR" or "LandR.CS"
# fire = "SCFM" or "fS"
# replicateNumber = i.e. "run1" or NULL for no id on the run

user <- pemisc::user()
whichComputer <- if (user == "tmichele") "BorealCloud" else "LocalMachine"

if (whichComputer == "BorealCloud" & basename(getwd()) != "NWT"){
  setwd(file.path(getwd(), "NWT"))
}

isTest <- FALSE

paths <- pathsSetup(whichComputer = whichComputer, isTest = isTest)
if (length(paths$modulePath) == 1) paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))
paths$outputPath <- checkPath(file.path(paths$outputPath, definedRun$whichRUN, replicate), create = TRUE) # Redefine outputPath based on type of run

if (pemisc::user() %in% c("Tati", "tmichele", "emcintir")) {
  setTempFolder(paths = paths, setTmpFolder = TRUE, usr = user)
}
maxMemory <- 5e+12
scratchDir <- checkPath(path = paste0("/mnt/tmp/rasterTMP/", user), create = TRUE)
#Here we check that the creation of the folder worked (we might have problems with writting access, only tested with my own user)
if(dir.create(scratchDir)) system(paste0("sudo chmod -R 777 /mnt/tmp/rasterTMP"), wait = TRUE) 
rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)

# Sessting up the cache folder: it is hosted in the project's GDrive
cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
tilePath <- file.path(paths$outputPath, "tiles")

# Setting all SpaDES options to be used in the project
.plotInitialTime <- NA
opts <- options(
  "spades.recoveryMode" = 2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = paths$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = "multicore",
  "future.globals.maxSize" = if (pemisc::user("tmichele")) 6000*1024^2 else 1000*1024^2,
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = FALSE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = paths$cachePath,
  "reproducible.showSimilar" = FALSE,
  "reproducible.useCloud" = FALSE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = FALSE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)

SpaDES.core::setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, cachePath = paths$cachePath)

# Check available memory
if (!exists("checkMemory")) checkMemory <- FALSE # Default if not provided
if (checkMemory){
  availableMem <- future::future(ongoingAvailableMemory(pathToSave = getPaths()$outputPath))
}

#################################################################################
################################## SIMULATION SET UP ############################
#################################################################################

library("googledrive")

tryCatch(googledrive::drive_download(file = googledrive::as_id("1EetOiGxAq-QTZCVU9Q6Y3I26tqjZm3Oi"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id("11CrK9PfNJzkU5cZg9-JYYzyr-VP23W0x"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})

inputs <- data.frame(
  files = c(file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds"), 
            file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")),
  functions = c("base::readRDS", "base::readRDS"),
  stringsAsFactors = FALSE
)

NWT.url <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
# EDE.url <- "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf"
studyArea <- Cache(prepInputs,
                   url = NWT.url,
                   destinationPath = getPaths()$inputPath[[1]],
                   userTags = "edeSA",
                   omitArgs = c("destinationPath"))

rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
                       studyArea = studyArea,
                       targetFile = "RTM.tif", destinationPath = getPaths()$inputPath[[1]],
                       filename2 = NULL,
                       userTags = "edeRTM",
                       omitArgs = c("destinationPath", "filename2"))

studyAreaPSP <- prepInputs(targetFile = "BCR6.tif",
                   archive = "BCR6.zip",
                   url = "https://drive.google.com/open?id=18A_HjSx_8viGxz3k87kjra03K6MgsjzO",
                   alsoExtract = "similar",
                   destinationPath = getPaths()$inputPath, userTags = c("objectName:studyAreaPSP", "extansion:BCR6"))

waterRaster <- prepInputs(url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM", 
                          destinationPath = getPaths()$inputPath, 
                          targetFile = "waterRasterNWT.tif",studyArea = studyArea,
                          rasterToMatch = rasterToMatch,
                          filename2 = NULL)

sppEquivCol <- "NWT"

# Equivalency table for tree species
data("sppEquivalencies_CA", package = "LandR")

# Make NWT spp equivalencies
sppEquivalencies_CA[, NWT := c(Betu_Pap = "Betu_Pap", 
                               Lari_Lar = "Lari_Lar", 
                               Pice_Gla = "Pice_Gla",
                               Pice_Mar = "Pice_Mar", 
                               Pinu_Ban = "Pinu_Ban", 
                               Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
# Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.

sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                 palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColorVect[length(sppColorVect)+1] <- mixed
attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

times <- list(start = 2011, end = 2100)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 

parameters <- list(
  #SCFM
  # ".progress" = list(type = "text", interval = 1),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NA
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(fireCause = "L"), #c("L", "H")
  scfmDriver = list(targetN = 1000), # 1500
  # LandR_Biomass
  LBMR = list(
    "successionTimestep" = 10,
    ".plotInitialTime" = times$end,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = definedRun$growthAndMortalityDrivers,
    ".useParallel" = 2),
  Boreal_LBMRDataPrep = list(
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(usefun::reviseSpeciesTraits(speciesTable = sim$species)),
      quote(usefun::changeTraits(speciesTable = sim$species, param = "seeddistance_max",
                                 facMult = 0.4, species = c("Betu_Pap", "Popu_Tre")))
    ),
    "useCloudCacheForStats" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 10,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50),
  Biomass_regeneration = list(
    "fireTimestep" = 1,
    "fireInitialTime" = times$start
  ),
  climate_NWT_DataPrep = list(
    "rcp" = 45, # 45 or 85
    "gcm" = "CanESM2"), # One of CanESM2, GFDL-CM3, HadGEM2-ES, MPI-ESM-LR
  fireSense_IgnitionPredict = list(
    "data" = c("MDC06", "LCC"),
    "modelObjName" = "fireSense_FrequencyFitted"),
  fireSense_EscapePredict = list(
    "data" = c("MDC06", "LCC")),
  fireSense_NWT_DataPrep = list(
    "train" = FALSE),
  # Caribou Population Growth
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useDummyData" = FALSE,
    ".growthInterval" = 5)
)

succTS <- c(seq(times$start, times$end, 
                by = parameters$LBMR$successionTimestep), times$end)
outputsLandR <- data.frame(
  objectName = rep(c("burnMap",
                     "cohortData",
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap",
                     "climateLayers",
                     "MDC06"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 9))
)
lastYears <- data.frame(objectName = c("predictedCaribou", "plotCaribou", 
                                       "fireRegimeRas", "speciesEcoregion", 
                                       "species", "gcsModel", "mcsModel"),
                        saveTime = times$end)
if (length(grepMulti(x = definedRun$modules, "LBMR")) != 0){
  clim <- data.frame(objectName = rep(c("fireSense_IgnitionPredicted", 
                                        "fireSense_EscapePredicted", "burnSummary"), 
                                      each = 3),
                     saveTime = rep(c(times$start, round((times$start + times$end)/2, 0), times$end), 
                                    times = 1))
} else {
  clim <- NULL
}

outputsLandR <- unique(rbind(outputsLandR, lastYears, clim))

objects <- list(
  "studyAreaPSP" = studyAreaPSP,
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColorVect,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster,
  "fireRegimePolys" = studyArea,
  "t1" = t1
)

data.table::setDTthreads(10) # Data.table has all threads by default, which is inconveninent and unecessary. Will try setting it for only 10 cores.  

if (!exists("runLandR")) runLandR <- FALSE # Default if not provided
if (runLandR){
  message(crayon::red(paste0("Starting simulations for ", definedRun$whichRUN)))
  assign(x = definedRun$whichRUN, simInitAndSpades(inputs = inputs, times = times,
                                                   params = parameters,
                                                   modules = definedRun$modules,
                                                   objects = objects,
                                                   paths = paths,
                                                   loadOrder = unlist(definedRun$modules),
                                                   outputs = outputsLandR, debug = 1))
  t2 <- Sys.time()
  message(crayon::green(paste0("Finished simulations for ", definedRun$whichRUN, ". Elapsed time: ", t2-t1)))
  saveRDS(object = get(definedRun$whichRUN),
          file = file.path(paths$outputPath, paste0(definedRun$whichRUN,
                                                    toupper(format(Sys.time(), "%d%b%y_%Hh%Mm%Ss")))))
  message(crayon::magenta(paste0("Saved simulations for ", definedRun$whichRUN, ". Elapsed time: ", Sys.time()-t2)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS

if (!exists("runBirds")) runBirds <- FALSE # Default if not provided
if (runBirds){
  predictionIntervals <- 30
  message(crayon::yellow(paste0("Starting simulations for BIRDS using ", definedRun$whichRUN)))

  # Passing the uplandsRaster here makes sure that all computers can use it as the operations 
  # to derive it from the DUCK's layer take up a lot of memory
  uplandsRaster <- Cache(prepInputs, targetFile = "uplandsNWT250m.tif", studyArea = studyArea, rasterToMatch = rasterToMatch,
                              url = "https://drive.google.com/open?id=1EF67NCH7HqN6QZ0KGlpntB_Zcquu6NJe", 
                              destinationPath = getPaths()$inputPath, filename2 = NULL, 
                         userTags = c("objectName:uplandsRaster", "goal:modelBirds"), omitArgs = c("userTags", "destinationPath"))
  parameters <- list(
    birdsNWT = list(
      "lowMem" = TRUE,
      "scenario" = paste(replicate, vegetation, fire, sep = "_"),
      "useStaticPredictionsForNonForest" = TRUE,
      "useOnlyUplandsForPrediction" = TRUE,
      "baseLayer" = 2005,
      "overwritePredictions" = FALSE,
      "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from LandR_Biomass simulations to run it with
      "useParallel" = FALSE, # Using parallel in windows is currently not working.
      "predictionInterval" = predictionIntervals,
      "quickLoad" = TRUE,
      "version" = 3 # VERSION 6 of the modules has both climate and vegetation as covariates for the model
    ),
    comm_metricsNWT = list(
    "frequency" = predictionIntervals
    )
  )
  objects <- c(objects, list(
    "birdsList" = c("BBWA", "CAWA", "OSFL", "WEWP", "RUBL"), # [ FIX ] <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ For second paper, just remove this line!
    "uplandsRaster" = uplandsRaster))
  modules <- list("birdsNWT", "comm_metricsNWT")
  
  invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/birdsNWT/R/"), full.names = TRUE), FUN = source))
  birdOutPath <- checkPath(file.path(paths$outputPath, paste0("birdPredictionsV", parameters[["birdsNWT"]][["version"]])), create = TRUE)
  setPaths(modulePath = paths$modulePath,
           cachePath = paths$cachePath,
           inputPath = paths$outputPath,
           outputPath = birdOutPath)
  
  # [ FIX ] only because we already ran LandR previously. Needs to be commented out when doing the whole project at once
  newInputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = "23OCT19")
  newOutputPath <- gsub(x = birdOutPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = "23OCT19")
  setPaths(inputPath = newInputPath,
           outputPath = newOutputPath)
  
  
  # For mem peak identification
  Require("future")
  # Require("future.callr")
  options("spades.memoryUseInterval" = 0.5, "spades.futurePlan" = "multicore")
  
  mySim <- simInit(
    inputs = inputs,
    times = times,
    params = parameters,
    modules = modules,
    objects = objects,
    paths = getPaths(),
    loadOrder = unlist(modules),
    outputs = outputsLandR
  )
  assign(x = paste0(definedRun$whichRUN, "_birds"), spades(mySim, debug = 1))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CARIBOU

if (!exists("runCaribou")) runCaribou <- FALSE # Default if not provided
if (runCaribou){
  message(crayon::white(paste0("Starting simulations for CARIBOUS using ", definedRun$whichRUN)))
  invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/caribouRSF/R/"), full.names = TRUE), FUN = source))
  caribouOutPath <- checkPath(file.path(paths$outputPath, "caribouPredictions"), create = TRUE)
  setPaths(modulePath = paths$modulePath, 
           cachePath = paths$cachePath,
           inputPath = paths$outputPath, 
           outputPath = caribouOutPath)
  
  # [ FIX ] only because we already ran LandR previously. Needs to be commented out when doing the whole project at once
  newInputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = "23OCT19")
  newOutputPath <- gsub(x = caribouOutPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = "23OCT19")
  setPaths(inputPath = newInputPath,
           outputPath = newOutputPath)
  
  parameters <- list(
    caribouRSF = list(
      "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
      "predictionInterval" = 30
    )
  )
  modules <- list("caribouRSF")
  assign(x = paste0(definedRun$whichRUN, "_caribou"), simInitAndSpades(inputs = inputs, times = times,
                                                                     params = parameters,
                                                                     modules = modules,
                                                                     objects = objects,
                                                                     paths = getPaths(),
                                                                     loadOrder = unlist(modules),
                                                                     outputs = outputsLandR, debug = 1))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~