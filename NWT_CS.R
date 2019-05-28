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

# Commits on 28th May - Whole NWT run
# Assumptions: 
# FIRE: fireSense runs a "climate sensitive" fire from 2001 to 2011 using the 2011 climate layer
# LANDR: Assumption at a landscape level is ok. We are assuming that you (we) are essentially making 
# the assumption that 2011 looked like 2001. At the _landscape_ level that's not a terrible assumption
# Oldest PSP stand is way younger than the data we have
# 35335d977c5e432fbaa7301bdcc8112b5d4dfc80 modules/Biomass_regeneration (heads/development)
# 019d688e1858792d3eddf3e9d1551a117d55535c modules/Boreal_LBMRDataPrep (heads/development)
# 84886af3a99fd341c7461158c1ae8c25a8f0c099 modules/LBMR (heads/development)
# a40bafbb4ea338f37ad8efdae06c2b32aad5f3af modules/LBMR2LCC_DataPrep (heads/master)
# 0d674d3a264e59148aa03fa4f3d004a007d115d2 modules/LandR_BiomassGMOrig (heads/development)
# 881957951841d667734556b84e39789622518343 modules/MDC_NWT_DataPrep (heads/master)
# 6e140843c8b307b69128289b8e377072b9265676 modules/PSP_Clean (heads/master)
# c120d8e1b106cd8d31e54f76bc07e8afbbf23309 modules/birdsNWT (heads/master)
# 1d1d04d410d55eb07961476dd97073918bb1e63a modules/caribouPopGrowthModel (heads/master)
# 7797b61b6291400fe3c709744ea367a33c072c3b modules/caribouRSF (heads/master)
# 4fc2e61cabf063722b17bb645ce0a180a3331899 modules/climate_NWT_DataPrep (heads/master)
# dba12450d340ef2a277cac00b633967745db0f80 modules/comm_metricsNWT (heads/master)
# d976b680f99d7a029dc216984c5fa96c96df42c4 modules/fireSense_EscapePredict (heads/master)
# 78ddd05efd17d9e9931201ee9449ba8dc99f18e7 modules/fireSense_FrequencyPredict (heads/master)
# dfc61faa8272e9c06c2f4ebeeada58d298c874cc modules/fireSense_NWT (heads/master)
# c3d3c54e82fbfb34ff53fe34fec9cc3ea8b0bec3 modules/fireSense_NWT_DataPrep (heads/master)
# 886184b6d832aee3a57186b53dec65eea1ae16ed modules/gmcsDataPrep (heads/development)
# 38557980e8675ce39aecafbd6fc6886a7a0cd57b modules/scfm (heads/development)

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

times <- list(start = 2001, end = 2100)

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
    # "growthInitialTime" = 2011, # Has a default to be start(sim) Maybe Ian changed it here when debugging?
    "successionTimestep" = 10,
    ".useParallel" = 3,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
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
    "fireInitialTime" = times$start
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
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 8))
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
 

NWT_CS <- simInitAndSpades(inputs, times = times,
                           params = parameters,
                           modules = modules,
                           objects = .objects, paths = paths,
                           loadOrder = unlist(modules),
                           outputs = outputsLandR, debug = 2)
