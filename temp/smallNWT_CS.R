
if (!pemisc::user("emcintir"))
  setwd("/mnt/data/Micheletti/NWT")

library("LandR")
library("SpaDES")
library("raster")
library("plyr"); library("dplyr")
library("magrittr") # for piping

updateAll <- FALSE
updateSubmodules <- FALSE

if (updateAll){
  devtools::install_github("PredictiveEcology/reproducible@development")
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
  update.packages(checkBuilt = TRUE)
}

# Source all common functions
invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))

user <- pemisc::user()
whichComputer <- if (user == "emcintir") "LocalMachine" else if (user == "tmichele") "BorealCloud" else "LocalMachine"

if (whichComputer == "BorealCloud" & basename(getwd()) != "NWT"){
  setwd(file.path(getwd(), "NWT"))
}

isTest <- FALSE

paths <- pathsSetup(whichComputer = whichComputer, isTest = isTest)
if (length(paths$modulePath) == 1) paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))

if (pemisc::user() %in% c("Tati", "tmichele")) {
  setTempFolder(paths = paths, setTmpFolder = TRUE, usr = user)
}
maxMemory <- 5e+12
scratchDir <- if (pemisc::user("emcintir")) {
  checkPath(path = paste0("~/HDD/tmp"), create = TRUE)
} else {
  checkPath(path = paste0("/mnt/tmp/rasterTMP/", user), create = TRUE)
}
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
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = paths$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = paths$cachePath,
  "reproducible.showSimilar" = FALSE,
  "reproducible.useCloud" = if (pemisc::user("emcintir")|pemisc::user("tmichele")) FALSE else TRUE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = FALSE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)

SpaDES.core::setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, cachePath = paths$cachePath)

if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)

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

# NWT.url <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
# EDE.url <- "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf"
smallArea.url <- "https://drive.google.com/open?id=1SUvMX4A5RJ057XYa2W_feX6P1L9As4g8"

# Small study area
studyArea <- Cache(prepInputs, url = smallArea.url, 
                   destinationPath = tempdir())

rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
                       studyArea = studyArea,
                       targetFile = "RTM.tif", destinationPath = getPaths()$inputPath[[1]],
                       filename2 = NULL,
                       userTags = "edeRTM",
                       omitArgs = c("destinationPath", "filename2"))

studyAreaPSP <- prepInputs(targetFile = "BC_Alberta.shp",
                           archive = "studyAreaPSP.zip",
                           url = "https://drive.google.com/open?id=19CkdAT0oaqHenyVK84-BjG0dT3JMarV0",
                           alsoExtract = "similar",
                           destinationPath = getPaths()$inputPath, userTags = "objectName:studyAreaPSP")

waterRaster <- prepInputs(url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM", 
                          destinationPath = getPaths()$inputPath, 
                          targetFile = "waterRasterNWT.tif",studyArea = studyArea,
                          rasterToMatch = rasterToMatch,
                          filename2 = NULL)

sppEquivCol <- "NWT"

# Equivalency table for tree species
data("sppEquivalencies_CA", package = "LandR")

# Make NWT spp equivalencies
# Popu_Tri == Popu_Bal in NWT
# Quer_mac in LandR needs to be Quer_Mac in NWT
sppEquivalencies_CA[, NWT := c(Abie_Bal = "Abie_Bal", 
                               Betu_Pap = "Betu_Pap", 
                               Lari_Lar = "Lari_Lar", 
                               Pice_Gla = "Pice_Gla",
                               Pice_Mar = "Pice_Mar", 
                               Pinu_Ban = "Pinu_Ban", 
                               # Pinu_Con = "Pinu_Con", 
                               # Popu_Bal = "Popu_Bal", 
                               Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
# Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.

sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                 palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColorVect[length(sppColorVect)+1] <- mixed
attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

modules <- c(
  "Boreal_LBMRDataPrep",
  "Biomass_regeneration",
  "LBMR",
  "climate_NWT_DataPrep",
  "MDC_NWT_DataPrep",
  "fireSense_NWT_DataPrep",
  "fireSense_FrequencyPredict",
  "fireSense_EscapePredict",
  "LBMR2LCC_DataPrep",
  "fireSense_NWT",
  "scfmLandcoverInit",
  "scfmRegime",
  "scfmDriver",
  "scfmSpread",
  "PSP_Clean",
  "gmcsDataPrep",
  "caribouPopGrowthModel"
)

times <- list(start = 0, end = 100)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 

parameters <- list(
  #SCFM
  ".progress" = list(type = "text", interval = 1),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NULL
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = times$start,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(fireCause = "L"), #c("L", "H")
  scfmDriver = list(targetN = 1000), # 1500
  # LandR_Biomass
  LBMR = list(
    "growthInitialTime" = 0,
    "successionTimestep" = 10,
    ".useParallel" = 3,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
    "successionTimestep" = 10,
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = "LandR.CS"),
  Boreal_LBMRDataPrep = list(
    "useCloudCacheForStats" = if (pemisc::user("tmichele")) FALSE else TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 10,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50),
  Biomass_regeneration = list(
    "fireInitialTime" = times$start,
    "fireTimestep" = defaultInterval,
    ".useCache" = c(".inputObjects")
    ),
  climate_NWT_DataPrep = list(
    "rcp" = 45, # 45 or 85
    "gcm" = "CanESM2"), # One of CanESM2, GFDL-CM3, HadGEM2-ES, MPI-ESM-LR
  fireSense_FrequencyPredict = list(
    "f" = 250 / 10000, # fireSense_FrequencyFit was fitted using the 10km resolution, predictions are made at the 250m resolution?
    "data" = c("MDC06", "LCC")),
  fireSense_EscapePredict = list(
    "data" = c("MDC06", "LCC")),
  fireSense_NWT_DataPrep = list(
    "train" = FALSE),
  # Caribou Population Growth
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useDummyData" = FALSE,
    ".growthInterval" = 10)
)

succTS <- seq(times$start, times$end, 
              by = parameters$LBMR$successionTimestep)
outputsLandR <- data.frame(
  objectName = rep(c("rstCurrentBurn",
                     "burnMap",
                     "cohortData",
                     "simulationTreeOutput",
                     "summaryBySpecies",
                     "summaryBySpecies1",
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 11))
)

.objects <- list(
  "studyAreaPSP" = studyAreaPSP,
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColorVect,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster
)
NWT_CS <- simInitAndSpades(inputs = inputs, times = times,
                           params = parameters,
                           modules = modules,
                           objects = .objects, paths = paths,
                           loadOrder = unlist(modules),
                           outputs = outputsLandR, debug = 2)
