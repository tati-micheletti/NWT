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
  devtools::install_github("PredictiveEcology/SpaDES.core@development") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("PredictiveEcology/SpaDES.tools@development") # Updates SpaDES.tools and SpaDES.core quickPlot
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
plan("multiprocess")

# Source all common functions
# invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))
source("/mnt/data/Micheletti/NWT/functions/not_included/pathsSetup.R")
source("/mnt/data/Micheletti/NWT/functions/defineRun.R")

if (!exists("vegetation")) vegetation <- "LandR" # Default if not provided
if (!exists("fire")) fire <- "SCFM" # Default if not provided
if (!exists("replicateNumber")) replicateNumber <- NULL # Default if not provided
if (!exists("runOnlySimInit")) runOnlySimInit <- FALSE # Default if not provided


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

if (user %in% c("tmichele", "Tati")) {
  paths <- pathsSetup(whichComputer = whichComputer, isTest = isTest)
} else {
  paths <- setPaths()
}

if (length(paths$modulePath) == 1) paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))
paths$outputPath <- checkPath(file.path(paths$outputPath, definedRun$whichRUN, replicateNumber), create = TRUE) # Redefine outputPath based on type of run
paths$outputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = "PAPER") # Added on 16th Jan after checking all went well.

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
  "reproducible.futurePlan" = FALSE,
  "future.globals.maxSize" = if (pemisc::user("tmichele")) 6000*1024^2 else 1000*1024^2,
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = paths$cachePath,
  "reproducible.showSimilar" = TRUE,
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
} # it is taking too long!

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

if (!exists("climateModel")) climateModel <- "CCSM4_85" # Default if not provided
if (!climateModel %in% c("CCSM4_85", "CCSM4_45")) stop("Other climate scenarios are still not implemented.")

cmi.url <- ifelse(climateModel == "CCSM4_85", "https://drive.google.com/open?id=1OcVsAQXKO4N4ZIESNmIZAI9IZcutctHX", 
                  "https://drive.google.com/open?id=1ERoQmCuQp3_iffQ0kXN7SCQr07M7dawv")
cmi.tf <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.grd", "Canada3ArcMinute_CCSM4_45_CMI2011-2100.grd")
cmi.arc <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.zip", "Canada3ArcMinute_CCSM4_45_CMI2011-2100.zip")
alsoExt <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.gri", "Canada3ArcMinute_CCSM4_45_CMI2011-2100.gri")

#CMIstack <- Cache(prepInputs, targetFile = cmi.tf,
#                           archive = cmi.arc,
#                           alsoExtract = alsoExt,
#                           url = cmi.url,
#                           destinationPath = file.path(getwd(), "modules/gmcsDataPrep/data"),
 #                          fun = "raster::stack", useCache = TRUE, 
  #                        userTags = c(paste0("climateModel:", climateModel), "CMI"),
   #                        omitArgs = c("destinationPath"))

ata.url <- ifelse(climateModel == "CCSM4_85", "https://drive.google.com/open?id=1jyfq-7wG4a7EoyNhirgMlq4mYnAvoOeY", 
                  "https://drive.google.com/open?id=1OA67hJDJunQbfeG0nvCnwd3iDutI_EKf")
ata.tf <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_ATA2011-2100.grd", "Canada3ArcMinute_CCSM4_45_ATA2011-2100.grd")
ata.arc <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_ATA2011-2100.zip", "Canada3ArcMinute_CCSM4_45_ATA2011-2100.zip")
alsoExt <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_ATA2011-2100.gri", "Canada3ArcMinute_CCSM4_45_ATA2011-2100.gri")

#ATAstack <- Cache(prepInputs, targetFile = ata.tf,
#                           archive = ata.arc,
#                           alsoExtract = alsoExt,
#                           url = ata.url,
#                           destinationPath = file.path(getwd(), "modules/gmcsDataPrep/data"),
#                           fun = "raster::stack", useCache = TRUE, 
#                           userTags = c(paste0("climateModel:", climateModel),"ATA"),
#                           omitArgs = c("destinationPath")) #if a pixel is 10 degrees above average, needs 4S

RCP <- ifelse(climateModel == "CCSM4_85", "85", "45") # 45
climateModelType = ifelse(climateModel == "CCSM4_85", "CCSM4", "CanESM2")# CanESM2 is NOT implemented yet. Here just figurative
ensemble <- ifelse(climateModel == "CCSM4_85", "", "r11i1p1")
climateResolution <- "3ArcMin" # Only available for now, matches the created layers for all modules
climateFilePath <- ifelse(climateModel == "CCSM4_85", "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", 
                          "https://drive.google.com/open?id=1U0TuYNMC75sQCkZs7c4EcBRLcjqeVO6N")

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
  LandR_speciesParameters = 
    list("sppEquivCol"  = sppEquivCol,
         "GAMMiterations" = 2, 
         "GAMMknots" = list(
           "Betu_Pap" = 3,
           "Lari_Lar" = 4,
           "Pice_Gla" = 3,
           "Pice_Mar" = 4,
           "Pinu_Ban" = 3,
           "Popu_Tre" = 4),
         "minimumPlotsPerGamm" = 40,
         "constrainMortalityShape" = list(
           "Betu_Pap" = c(15,25),
           "Lari_Lar" = c(20,25),
           "Pice_Gla" = c(15,25),
           "Pice_Mar" = c(15,25),
           "Pinu_Ban" = c(15,25),
           "Popu_Tre" = c(15,25)
         ),
         "quantileAgeSubset" = list(
           "Betu_Pap" = 95,
           "Lari_Lar" = 95,
           "Pice_Gla" = 95,
           "Pice_Mar" = 95,
           "Pinu_Ban" = 95,
           "Popu_Tre" = 99
         )
    ),
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
  scfmRegime = list(fireCause = "L"),
  scfmDriver = list(
    targetN = 1000),
  # LandR_Biomass
  LBMR = list(
    "successionTimestep" = 10,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = definedRun$growthAndMortalityDrivers,
    ".useParallel" = 2),
  Boreal_LBMRDataPrep = list(
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
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
  gmcsDataPrep = list(
     "GCM" = "CCSM4_RCP8.5"),
  fireSense_IgnitionPredict = list(
    "data" = c("MDC06", "LCC"),
    "modelObjName" = "fireSense_FrequencyFitted"),
  fireSense_EscapePredict = list(
    "data" = c("MDC06", "LCC")),
  fireSense_NWT_DataPrep = list(
    "train" = FALSE,
    "RCP" = RCP,
    ".useCache" = c(".inputObjects"),
    "climateModel" = climateModelType,
    "ensemble" = ensemble,
    "climateResolution" = climateResolution,
    "climateFilePath" = climateFilePath),
  # Caribou Population Growth
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useCache" = c(".inputObjects"),
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
                                        "fireSense_EscapePredicted", "burnSummary", 
                                        "successionLayers", "activePixelIndex"), 
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

if (runOnlySimInit){
  spadesFun <- "simInit"
} else {
  spadesFun <- "simInitAndSpades"
}

if (!exists("runLandR")) runLandR <- FALSE # Default if not provided
if (runLandR){
  message(crayon::red(paste0("Starting ", ifelse(runOnlySimInit, "simInit", "simulations"), " for ", definedRun$whichRUN, " ", definedRun$whichReplicate)))
  assign(x = definedRun$whichRUN, do.call(get(spadesFun), args = list(inputs = inputs, times = times,
                                                   params = parameters,
                                                   modules = definedRun$modules,
                                                   objects = objects,
                                                   paths = getPaths(),
                                                   loadOrder = unlist(definedRun$modules),
                                                   outputs = outputsLandR)))
  t2 <- Sys.time()
  message(crayon::green(paste0("Finished ", ifelse(runOnlySimInit, "simInit", "simulations")," for ", definedRun$whichRUN, ". Elapsed time: ", t2-t1)))
  if (!runOnlySimInit){
    saveRDS(object = get(definedRun$whichRUN),
            file = file.path(getPaths()$outputPath, paste0(definedRun$whichRUN,
                                                      toupper(format(Sys.time(), "%d%b%y_%Hh%Mm%Ss")))))
    message(crayon::magenta(paste0("Saved simulations for ", definedRun$whichRUN, ". Elapsed time: ", Sys.time()-t2)))
rm(list = definedRun$whichRUN)
gc()
  } # End !runOnlySimInit
} # End runLandR

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS MODEL 1

if (!exists("runBirds")) runBirds <- FALSE # Default if not provided
if (runBirds){
  if (!exists("birdModelVersion")) birdModelVersion <- 6 # Default if not provided
  predictionIntervals <- 20
  message(crayon::yellow(paste0("Starting simulations for BIRDS using ", definedRun$whichRUN, " ", definedRun$whichReplicate)))

  # Passing the uplandsRaster here makes sure that all computers can use it as the operations
  # to derive it from the DUCK's layer take up a lot of memory
  uplandsRaster <- Cache(prepInputs, targetFile = "uplandsNWT250m.tif", studyArea = studyArea, rasterToMatch = rasterToMatch,
                              url = "https://drive.google.com/open?id=1EF67NCH7HqN6QZ0KGlpntB_Zcquu6NJe", 
                              destinationPath = getPaths()$inputPath, filename2 = NULL, 
                         userTags = c("objectName:uplandsRaster", "goal:modelBirds"), omitArgs = c("userTags", "destinationPath"))
  bMod <- ifelse(length(birdModelVersion) == 1, birdModelVersion, birdModelVersion[1])
  parameters <- list(
    birdsNWT = list(
      "lowMem" = TRUE,
      "scenario" = paste(replicateNumber, vegetation, fire, sep = "_"), # THIS IS THE CORRECT
      "useStaticPredictionsForNonForest" = TRUE,
      "useOnlyUplandsForPrediction" = TRUE,
      "baseLayer" = 2005,
      "overwritePredictions" = FALSE,
      "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from LandR_Biomass simulations to run it with
      "useParallel" = FALSE, # Using parallel in windows is currently not working.
      "predictionInterval" = predictionIntervals,
      "quickLoad" = TRUE,
      "version" = bMod, # VERSION 6 of the modules has both climate and vegetation as covariates for the model
      "RCP" = RCP,
      "climateModel" = climateModelType,
      "ensemble" = ensemble,
      "climateResolution" = climateResolution,
      "climateFilePath" = climateFilePath),
    comm_metricsNWT = list(
    "frequency" = predictionIntervals
    )
  )
  objects <- c(objects, list(
    "birdsList" = c("CAWA", "OSFL", "RUBL"), # [ FIX ] <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ For second paper, just remove this line!"BBWA", "WEWP", 
    "uplandsRaster" = uplandsRaster,
    "climateDataFolder" = getPaths()$inputPath,
    "pixelsWithDataAtInitialization" = tryCatch(readRDS(file.path(getPaths()$inputPath, "pixelsWithDataAtInitialization.rds")), 
                                                error = function(e){
                                                  warning(paste0("The pixelsWithDataAtInitialization.rds object was not found. Returning NULL.",
"This will affect bird predictions for the NWT (i.e. density will not be predicted",
"for pixels were total biomas = 0). To fix this, save the object named 'sim$activePixelIndex' in the end of LBMR's init i.e. add to LBMR's Line 661-662 ",
"saveRDS(sim$activePixelIndex, file = file.path(outputPath(sim), 'pixelsWithDataAtInitialization.rds'))"))
                                                  })))
  modules <- list("birdsNWT") # [ FIX ] <~~~~~~~~~~~~~~~~~ For second paper, add community metrics!
  
  invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/birdsNWT/R/"), full.names = TRUE), FUN = source))

#[ FIX ] only because we already ran LandR previously. Needs to be commented out when doing the whole project at once
  if (all(runLandR == FALSE)){
    if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the analysis (i.e. where LandR results are)")
    newInputPath <- gsub(x = getPaths()$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = originalDateAnalysis)
    newOutputPath <- gsub(x = getPaths()$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = originalDateAnalysis)
    setPaths(inputPath = newInputPath,
             outputPath = newOutputPath)
    birdOutPath <- checkPath(file.path(newOutputPath, 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), create = TRUE)
    setPaths(outputPath = birdOutPath)
  } else {
    birdOutPath <- checkPath(file.path(getPaths()$outputPath, 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), create = TRUE)
    setPaths(inputPath = getPaths()$outputPath,
             outputPath = birdOutPath)
  }
  
    assign(x = paste0(definedRun$whichRUN, "_birdsV", parameters[["birdsNWT"]][["version"]]), do.call(get(spadesFun), args = list(inputs = inputs,
                                                                                                      times = times,
                                                                                                      params = parameters,
                                                                                                      modules = modules,
                                                                                                      objects = objects,
                                                                                                      paths = getPaths(),
                                                                                                      loadOrder = unlist(modules),
                                                                                                      outputs = outputsLandR)))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS MODEL 2
    
    if (length(birdModelVersion) > 1){ # Run a second bird model
      bMod <- birdModelVersion[2]
      parameters[["birdsNWT"]][["version"]] <- bMod
      birdOutPath <- checkPath(file.path(dirname(getPaths()$outputPath), 
                                         paste0("birdPredictionsV", 
                                                parameters[["birdsNWT"]][["version"]])), create = TRUE)
      setPaths(outputPath = birdOutPath)
      assign(x = paste0(definedRun$whichRUN, "_birdsV",
                        parameters[["birdsNWT"]][["version"]]),
             do.call(
               get(spadesFun),
               args = list(
                 inputs = inputs,
                 times = times,
                 params = parameters,
                 modules = modules,
                 objects = objects,
                 paths = getPaths(),
                 loadOrder = unlist(modules),
                 outputs = outputsLandR
               )
             ))
    } # End of second bird model
} # End of run birds

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CARIBOU

if (!exists("runCaribou")) runCaribou <- FALSE # Default if not provided
if (runCaribou){
  message(crayon::white(paste0("Starting simulations for CARIBOUS using ", definedRun$whichRUN, " ", definedRun$whichReplicate)))
  if (all(runLandR == FALSE, runBirds == FALSE)){
    if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the analysis (i.e. where LandR results are)")
    newInputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = originalDateAnalysis)
    newOutputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = originalDateAnalysis)
    setPaths(inputPath = newInputPath,
             outputPath = newOutputPath)
  } else {
    if (runBirds == TRUE){ # input path is correct, independently if I ran LandR before birds
      caribouOutPath <- checkPath(file.path(dirname(getPaths()$outputPath), "caribouPredictions"), create = TRUE)
      setPaths(outputPath = caribouOutPath)
    } else { # only if I didn't run birds, only LandR
      caribouOutPath <- checkPath(file.path(dirname(getPaths()$outputPath), "caribouPredictions"), create = TRUE)
      setPaths(inputPath = getPaths()$outputPath,
               outputPath = birdOutPath)
    }
  }
    
  invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/caribouRSF/R/"), full.names = TRUE), FUN = source))

  parameters <- list(
    caribouRSF = list(
      "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
      "predictionInterval" = 20
    )
  )
  modules <- list("caribouRSF")
  assign(x = paste0(definedRun$whichRUN, "_caribou"), do.call(get(spadesFun), args = list(inputs = inputs,
                                                                                                    times = times,
                                                                                                    params = parameters,
                                                                                                    modules = modules,
                                                                                                    objects = objects,
                                                                                                    paths = getPaths(),
                                                                                                    loadOrder = unlist(modules),
                                                                                                    outputs = outputsLandR, debug = 1)))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
