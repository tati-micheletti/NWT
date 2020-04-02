#if (TRUE) { # set this to FALSE to jump directly to simInitAndSpades of fireSense_SpreadFit
# Before running this script, read !sourceScript to know the 4 
# parameters that needed to define the run 

# googledrive::drive_auth(use_oob = TRUE) # USE ONLY ONCE, the first time you are running the project 
# USING RStudio Server.

usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else "eliotmcintire@gmail.com"
googledrive::drive_auth(email = usrEmail)

if (pemisc::user() %in% c("Tati", "tmichele"))
  setwd("/mnt/data/Micheletti/NWT")
updateCRAN <- FALSE
updateGithubPackages <- FALSE
updateSubmodules <- FALSE
prepCohortData <- TRUE # Preamble. If already ran (i.e. objs cohortData2011 and cohortData2001 
# exist in inputs folder) this should NOT be run i.e. FALSE)

if (updateCRAN)
  update.packages(checkBuilt = TRUE, ask = FALSE)

if (updateGithubPackages){
  devtools::install_github("PredictiveEcology/reproducible@messagingOverhaul")
  devtools::install_github("tati-micheletti/usefun") # Updates LandR
  devtools::install_github("achubaty/amc@development")
  devtools::install_github("PredictiveEcology/pemisc@development")
  devtools::install_github("PredictiveEcology/map@development")
  devtools::install_github("PredictiveEcology/SpaDES.core@lowMemory") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("PredictiveEcology/SpaDES.tools@allowOverlap") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("PredictiveEcology/LandR@reworkCohorts") # Updates SpaDES.tools and SpaDES.core quickPlot
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
library("data.table")
library("LandR")
library("LandR.CS")
library("SpaDES")
library("SpaDES.experiment")
library("raster")
library("plyr"); library("dplyr")
library("amc")
library("magrittr") # for piping
library("future")
library("future.apply")

# Source all common functions
# invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))
source("functions/not_included/pathsSetup.R")
source("functions/defineRun.R")

if (!exists("vegetation")) vegetation <- "LandR" # Default if not provided
if (!exists("fire")) fire <- "SCFM" # Default if not provided
if (!exists("replicateNumber")) replicateNumber <- "run1" # Default if not provided #TOCHANGE
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
  paths <- setPaths(modulePath = file.path(getwd(), "modules"),
                    outputPath = file.path(getwd(), "outputs"),
                    cachePath = file.path(getwd(), "cache"))
  paths <- getPaths()
}

if (length(paths$modulePath) == 1) paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))
paths$outputPath <- checkPath(file.path(paths$outputPath, definedRun$whichRUN, replicateNumber), create = TRUE) # Redefine outputPath based on type of run
paths$outputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), replacement = "fireSenseTESTS") # Added on 16th Jan after checking all went well.

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
  "spades.recoveryMode" = 2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = paths$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "future.globals.maxSize" = if (pemisc::user("tmichele")) 6000*1024^2 else 1000*1024^2,
  "reproducible.cacheSaveFormat" = "qs",
  "reproducible.qsPreset" = "fast",
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
  "spades.useRequire" = TRUE, # assuming all pkgs installed correctly
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

studyAreaPSP <- Cache(prepInputs, targetFile = "BCR6.tif",
                      archive = "BCR6.zip",
                      url = "https://drive.google.com/open?id=18A_HjSx_8viGxz3k87kjra03K6MgsjzO",
                      alsoExtract = "similar",
                      destinationPath = getPaths()$inputPath,
                      userTags = c("objectName:studyAreaPSP", "extansion:BCR6"))

waterRaster <- Cache(prepInputs,
                     url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM",
                     destinationPath = getPaths()$inputPath,
                     targetFile = "waterRasterNWT.tif", studyArea = studyArea,
                     rasterToMatch = rasterToMatch,
                     filename2 = NULL)

# Original script for making the ecoRegion raster. After making it, uploaded a NWT version of it and using prepInputs
# ecoDistrict <- Cache(prepInputs,
#                      targetFile = "ecoregions.shp",
#                      archive = "ecoregion_shp.zip",
#                      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
#                      alsoExtract = "similar",
#                      destinationPath = Paths$inputPath,
#                      studyArea = studyArea,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
#                      useSAcrs = TRUE, # this is required to make ecoZone be in CRS of studyArea
#                      fun = "raster::shapefile",
#                      # filename2 = TRUE,
#                      userTags = c("prepInputsEcoRegion_SA", "where:fromGlobal"), # use at least 1 unique userTag
#                      omitArgs = c("destinationPath", "targetFile", "overwrite", "alsoExtract", "userTags"))
# library(fasterize)
# library(sf)
# ecoDistrictSF <- st_as_sf(ecoDistrict)
# ecoDistrictRAS <- fasterize::fasterize(sf = ecoDistrictSF, raster = rasterToMatch, field = "ECOREGION")
# ecoRegionRAS <- postProcess(ecoDistrictRAS, studyArea = studyArea, 
#                             rasterToMatch = rasterToMatch,
#                             destinationPath = Paths$inputPath,
#                             userTags = c("prepInputsEcoRegion_RAS", "where:fromGlobal"), # use at least 1 unique userTag
#                             omitArgs = c("destinationPath", "overwrite"))
# ecoRegionRAS[ecoRegionRAS == 0] <- NA
# saveRDS(ecoRegionRAS, file.path(Paths$inputPath, "ecoRegionRAS.rds"))
# drive_upload(file.path(Paths$inputPath, "ecoRegionRAS.rds"), as_id("1pYtRYDwQQi41rmx11sw0LxR1QBT3mYTv"))

ecoRegionRAS <- Cache(prepInputs,
                      targetFile = "ecoRegionRAS.rds",
                      url = "https://drive.google.com/open?id=1AT2yWbFjAKbY8nPhnwNJ6oviK94_92Pk",
                      alsoExtract = "similar",
                      destinationPath = Paths$inputPath,
                      studyArea = studyArea,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                      fun = "readRDS",
                      userTags = c("prepInputsEcoRegion_RAS", "where:fromGlobal"), # use at least 1 unique userTag
                      omitArgs = c("destinationPath",
                                   "targetFile",
                                   "overwrite",
                                   "userTags"))

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

times <- list(start = 2011, end = 2022)

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
  Biomass_core = list(
    "successionTimestep" = 10,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = TRUE,
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = definedRun$growthAndMortalityDrivers,
    ".useParallel" = 2),
  Biomass_borealDataPrep = list(
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(usefun::changeTraits(speciesTable = sim$species, param = "seeddistance_max",
                                 facMult = 0.4, species = c("Betu_Pap", "Popu_Tre")))
    ),
    "useCloudCacheForStats" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 20,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50#,
    #"biomassModel" = quote(glm(B ~ logAge * speciesCode * cover * ecoregionGroup))
  ),
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
                by = parameters$Biomass_core$successionTimestep), times$end)
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
if (length(grepMulti(x = definedRun$modules, "Biomass_core")) != 0){
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
  "rasterToMatchLarge" = rasterToMatch,
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "studyArea" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColorVect,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster,
  "fireRegimePolys" = studyArea,
  "ecoregionRst" = ecoRegionRAS
)

data.table::setDTthreads(10) # Data.table has all threads by default, which is inconveninent and unecessary. Will try setting it for only 10 cores.  

if (runOnlySimInit){
  spadesFun <- "simInit"
} else {
  spadesFun <- "simInitAndSpades"
}

#########################################################
##                   PREAMBLE                          ##
#########################################################

if (prepCohortData){
  tempPaths <- getPaths()
  tempPaths$outputPath <- tempPaths$inputPath
  outputsPreamble <- data.frame(objectName = c("cohortData", "pixelGroupMap"),
                                saveTime = 2001,
                                file = c("cohortData2001_fireSense.rds", "pixelGroupMap2001_fireSense.rds"))
  
  # 1. Run borealBiomassDataPrep ALONE and save: cohortData + pixelGroupMap: will be used 
  # in fireSense_SizeFit and fireSense_SpreadFit (later on, will be also used in Ignition and Escape fits)
  # 271 unique using ecoregion
  # 973 unique using ecodistrict
  
  biomassMaps2001 <- Cache(simInitAndSpades, times = list(start = 2001, end = 2001),
                           params = parameters,
                           modules = list("Biomass_borealDataPrep"),
                           objects = objects,
                           paths = tempPaths, 
                           #useCloud = TRUE, cloudFolderID = cloudFolderID,
                           loadOrder = "Biomass_borealDataPrep",
                           outputs = outputsPreamble, #clearSimEnv = TRUE,
                           userTags = c("objective:preambleBiomassDataPrep", "time:year2001", "version:fixedZeros"))
  # 2. Load these:
  speciesLayers2011 <- Cache(loadkNNSpeciesLayersValidation,
                             dPath = Paths$inputPath,
                             rasterToMatch = rasterToMatch,
                             studyArea = studyArea,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                             sppEquiv = sppEquivalencies_CA,
                             knnNamesCol = "KNN",
                             sppEquivCol = sppEquivCol,
                             thresh = 10,
                             url = "http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/",
                             userTags = c("preamble", "speciesLayers2011"))
  
  outputsPreamble <- data.frame(objectName = c("cohortData", "pixelGroupMap"),
                                saveTime = 2011,
                                file = c("cohortData2011_fireSense.rds", "pixelGroupMap2011_fireSense.rds"))
  
  objectsPre <- objects
  objectsPre$speciesLayers <- speciesLayers2011
  
  # and pass as object to a second call of Biomass_borealDataPrep. Save cohortData + pixelGroupMap.
  biomassMaps2011 <- Cache(simInitAndSpades, times = list(start = 2011, end = 2011),
                           params = parameters,
                           modules = list("Biomass_borealDataPrep"),
                           objects = objectsPre,
                           paths = tempPaths, #useCache = "overwrite",
                           loadOrder = "Biomass_borealDataPrep",
                           clearSimEnv = TRUE,
                           outputs = outputsPreamble,
                           userTags = c("objective:preambleBiomassDataPrep", "time:year2011"))
  
}

######################## Create dataset for SpreadFit ########################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ fireAttributesFireSense_SpreadFit

# Load these so I can use as rasterToMatch 
#TODO Wrap this up in a tryCatch + a redo statement

# 2001
cohortData2001 <- readRDS(file.path(Paths$inputPath, "cohortData2001_fireSense_year2001.rds"))
pixelGroupMap2001 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2001_fireSense_year2001.rds"))

# 2011
cohortData2011 <- readRDS(file.path(Paths$inputPath, "cohortData2011_fireSense_year2011.rds"))
pixelGroupMap2011 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2011_fireSense_year2011.rds"))

# After getting the fire, I should get the weather (MDC)
# I downloaded the data manually using climateNA and placed in the /inputs folder
source("functions/calculateMDC.R")

fireYears <- 1991:2017 
names(fireYears) <- as.character(fireYears)
# plan("multiprocess", workers = length(fireYears))
if (!file.exists(file.path(Paths$inputPath, "MDC_1991_2017.rds"))){
  MDC <- Cache(calculateMDC, pathInputs = file.path(Paths$inputPath, "NWT_3ArcMinuteM"), 
               years = c(fireYears), doughtMonths = 4:9, rasterToMatch = pixelGroupMap2001, 
               userTags = c("MDC_1991_2017", "normals_MDC"))
  
  saveRDS(MDC, file.path(Paths$inputPath, "MDC_1991_2017.rds"))
} else {
  MDC <- readRDS(file.path(Paths$inputPath, "MDC_1991_2017.rds"))
}

# Here I create the dataset of initial fire locations for fireSense_SpreadFit
# We need a dataframe of the origin of the fire (coordinates) and each fire size (as a data.frame)

# 1. To get the origin of the fire:
source(file.path(getwd(), "functions/getFirePoints_NFDB.R"))
fireLocationsPoints <- Cache(getFirePoints_NFDB,
                             url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
                             studyArea = studyArea, rasterToMatch = rasterToMatch,
                             NFDB_pointPath = file.path(Paths$inputPath, "NFDB_point"),
                             userTags = c("what:firePoints", "forWhat:fireSense_SpreadFit"))
fireLocationsPoints <- fireLocationsPoints[fireLocationsPoints$YEAR <= max(fireYears) & 
                                             fireLocationsPoints$YEAR >= min(fireYears),]
fireLocationsPoints <- fireLocationsPoints[, c("YEAR", "SIZE_HA")]
fireLocationsPoints$fireSize <- asInteger(fireLocationsPoints$SIZE_HA / prod(res(rasterToMatch)) * 1e4)
names(fireLocationsPoints) <- c("date", "size_ha", "size")

# bigger than 1 pixel
fireLocationsPoints <- fireLocationsPoints[fireLocationsPoints$size > 1,]
fireAttributesFireSense_SpreadFit <- fireLocationsPoints

rasterTemp <- setValues(pixelGroupMap2001, values = 1:ncell(pixelGroupMap2001))

if (FALSE) {
  # 2. To get the coordinates:
  startingPointsCoord <- data.table(coords = coordinates(fireLocationsPoints)[, 1:2],
                                    year = fireLocationsPoints$YEAR,
                                    fireID = fireLocationsPoints$FIRE_ID,
                                    fireSize = asInteger(fireLocationsPoints$SIZE_HA/
                                                           prod(res(rasterToMatch))*1e4))
  
  # Subsetting for the same period we have the dataset
  startingPointsCoord <- startingPointsCoord[year <= max(fireYears) & year >= min(fireYears)]
  setDT(startingPointsCoord)
  setnames(startingPointsCoord, old = colnames(startingPointsCoord)[1:3],
           new = c("x", "y", "year"))
  
  # 3. To get the pixelID based on the coordinates:
  pixelID_fireStarts <- startingPointsCoord[
    , `:=`(pixelID = raster::extract(rasterTemp, coordinates(startingPointsCoord[, c("x", "y")])),
           origin = TRUE)]
  
  # 4. To get fire size based on the coordinates pixelID from cohort_Fire:
  cohort_starts <- merge(cohort_Fire, pixelID_fireStarts, by = "pixelID", all.y = TRUE)
  cohort_startsRed <- na.omit(cohort_starts)
  colsToKeep <- c("year", "fireSize", "pixelID")
  colsToDel <- setdiff(names(cohort_starts), colsToKeep)
  cohort_startsRed <- cohort_startsRed[, c(colsToDel) := NULL]
  cohort_startsRed <- unique(cohort_startsRed)
  
  # We have the same pixel burning in different years... 
  #ids <- duplicated(cohort_startsRed$pixelID)
  #Reps <- cohort_startsRed[ids, pixelID]
  # cohort_startsRed[pixelID %in% Reps, ]
  
  # 5. Extract coordinates of the pixelID's I have in cohort_startsRed
  coordins <- raster::xyFromCell(object = rasterTemp, cell = cohort_startsRed$pixelID)
  testthat::expect_true(NROW(cohort_startsRed) == NROW(coordins))
  
  fireAttributesFireSense_SpreadFit <- 
    SpatialPointsDataFrame(coordins, data = data.frame(size = cohort_startsRed$fireSize,
                                                       date = cohort_startsRed$year))
}

crs(fireAttributesFireSense_SpreadFit) <- crs(rasterTemp)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ dataFireSense_SpreadFit

# RasterStack of the model covariates. 
# The number of layers in the RasterStack should equal the number of distinct dates in column 'date'.
# Each COVARIATE of the model is ONE different raster stack containing that variable for all the different years.
# TODO: To include a bit more stochasticity, simulate changes from 2001 until 2011, and from 2011 until 2017 using 2001 and 2011 layers, respectively and the fire dataset (I know exactly where the fires occurred and how big they were! I just need to call them in a module each year) . At each each I save cohortData and pixelGroupMap, and using rasterizeReduce, I create the proportional biomass layers for each year for each class (would have to reclassify the species).

# For now, however, we will assume the landscape and proportions of species don't change for @ 15 years, and we use all the dataset 1991-2017 to fit the spread model, using 2001 and 2011 layers
# 1991:2004 --> 2001: cohortData2001
# 2005:2017 --> 2011: cohortData2011

# Create the classification. Repeat with 2011
source(file.path(getwd(), 'functions/classifyCohortsFireSenseSpread.R'))
classList2001 <- classifyCohortsFireSenseSpread(cohortData2001, year = 2001)
classList2011 <- classifyCohortsFireSenseSpread(cohortData2011, year = 2011)


# To create the class5, I need to do 1-sum(class1:4)
class5_2001 <- calc(x = stack(classList2001), fun = sum, na.rm = TRUE)
classList2001[["class5"]] <- 1 - class5_2001
names(classList2001[["class5"]]) <- "class5_2001"
class5_2011 <- calc(x = stack(classList2011), fun = sum, na.rm = TRUE)
classList2011[["class5"]] <- 1 - class5_2011
names(classList2011[["class5"]]) <- "class5_2011"

#class1 <- class2 <- class3 <- class4 <- class5 <- list()

# Assign values from 2001 and 2011 veg input layers to annual data
yearToDivide <- 2005
if (FALSE) {
  numClasses <- length(classList2001)
  fireYearsList <- split(fireYears, f = fireYears >= yearToDivide) # because fireYears has names, it keeps them
  classList <- append(rep(classList2001, length(fireYearsList[[1]])), 
                      rep(classList2011, length(fireYearsList[[2]])))
  # make 2 level list -- year on outside
  classList <- split(classList, f = rep(fireYears, each = numClasses))
} else {
  classList <- list(classList2001, classList2011)
  names(classList) <- c(paste0(fireYears[fireYears < yearToDivide], collapse = "_"),
                        paste0(fireYears[fireYears >= yearToDivide], collapse = "_"))
  
}
# classList <- purrr::transpose(classList)

# classesList <- lapply(paste0("class", 1:5), function(cl){
#   classYear <- lapply(fireYears, function(i) {
#     if (i < 2005){
#       assign(cl, classList2001[[cl]])
#     } else {
#       assign(cl, classList2011[[cl]])
#     }
#   })
#   names(classYear) <- as.character(fireYears)
#   classYear <- lapply(names(classYear), function(clsYrNm){
#     names(classYear[[clsYrNm]]) <- clsYrNm
#     return(classYear[[clsYrNm]])
#   })
#   names(classYear) <- as.character(fireYears)
#   #names(classYear) <- paste(cl, fireYears, sep = "_")
#   return(classYear)
# })
# names(classesList) <- paste0("class", 1:5)
# 
# # lapply through names(classesList) clsNames, stack and assign each stack to clsNames
# env <- environment()
# invisible(lapply(names(classesList), function(clsNames){
#   stk <- raster::stack(classesList[[clsNames]])
#   assign(x = clsNames, value = stk, envir = env)
# }))

# pull to memory
stackToMemory <- function (x, ...) 
{
  r <- stack(x, ...)
  r <- setValues(r, getValues(r))
  return(r)
}

weather <- Cache(stackToMemory, MDC)
weather <- raster::unstack(weather)
names(weather) <- as.character(fireYears)

# weave all covariates together
annualRasters <- mapply(c, weather = weather, SIMPLIFY=FALSE)
annualStacks <- lapply(annualRasters, raster::stack)
rm(annualRasters)

nonAnnualRasters <- mapply(c, classList, SIMPLIFY=FALSE)
nonAnnualStacks <- lapply(nonAnnualRasters, raster::stack)
rm(nonAnnualRasters)

# pixelIDLociYear <- data.table(pixelID = raster::extract(rasterTemp, coordinates(startingPointsCoord[, c("x", "y")])),
#                                  year = startingPointsCoord$year)
# Think I have this already...

source("functions/getFirePolygons.R")
#firePolys <- Cache(getFirePolygons, years = fireYears, studyArea = studyArea, 
#                            pathInputs = Paths$inputPath, userTags = c("years:1991_2017"))


####################### Finished dataset for SpreadFit #######################

modules <- list("fireSense_SpreadFit")

times <- list(start = 1, end = 1)

objects <- list(annualStacks = annualStacks, 
                nonAnnualStacks = nonAnnualStacks,
                rasterToMatch = rasterToMatch,
                fireAttributesFireSense_SpreadFit = fireAttributesFireSense_SpreadFit)
rm(annualStacks, weather, nonAnnualStacks, classList, classList2001, objectsPre)
#   class1 = class1,
# class2 = class2,
# class3 = class3,
# class4 = class4,
# class5 = class5,
# weather = weather)

# Define fireSense_SpreadFit module parameters
# formula <- formula(~ I(1/beta) + log(theta) - 1) # For when doing SizeFit/Predict
formula <- formula(~ 0 + weather + class1 + class2 + class3 + class4 + class5) # For when not doing SizeFit/Predict

# The parameters for
# weather : need to be positive, but very low... as the sum of all needs to be < 0.245
# class1 : needs to be negative, as it influences negatively the fire
# class2 : needs to be negative, as it influences negatively the fire
# class3 : needs to be positive, as fire spreads through conifers
# class4 : needs to be positive + , as fire spreads through Jack Pine even better
# class 5 : can be either...

# IDEAS:

# Rasterize the fire polygons with what burned and what didn't
# For each set of parameters, we
# Inside the ibj fun: generate the landscape, and spread 1000 times, convert those 1000 maps in the probability of being burned. # eliot will help with spread
# And compare (take the sum of the negative and log of the probabilities) each pixel coming from these 1000 burned prob map with the historical burned using bernoulli ()

# RESCALE MDC 
# Should let the classes take 100% of it if needs
lowerParams <- c(0, 0.001, 0.001, 0.001, 0.001, 0.001)
upperParams <- c(0.05, 0.1, 0.1, 0.1, 0.1, 0.1)

parameters <- list(
  fireSense_SpreadFit = list(
    formula = formula, # Formula of the statistical model
    fireAttributesFireSense_SpreadFit = "fireAttributesFireSense_SpreadFit", # Default
    # data = c("beta", "theta"),
    data = c("weather", "class1", "class2", "class3", "class4", "class5"),
    # Here are the bounds for: 5 parameters for log fun + n parameters for the model (i.e. n terms of a formula)
    #  lower asymptote, upper asymptote, (inflection point), slope at inflection pt, asymmetry
    lower = c(0.02, 0.22, 0.1, 0.5, lowerParams),
    upper = c(0.15, 0.3, 10, 4, upperParams),
    cores = 50, #pemisc::makeOptimalCluster(useParallel = TRUE)
    iterDEoptim = 100,
    verbose = TRUE,
    trace = 1,
    termsNAtoZ = c(paste0("class", 1:5))
  )
)
#}
# Run the simulation
sim <- simInitAndSpades(
  inputs = inputs,
  modules = modules,
  objects = objects,
  params = parameters,
  times = times
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> HERE

fireSense_SpreadFitted <- sim$fireSense_SpreadFitted # Extract the fitted model from the sim object


#########################################################
##                      RUNS                           ##
#########################################################


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
"for pixels were total biomas = 0). To fix this, save the object named 'sim$activePixelIndex' in the end of Biomass_core's init i.e. add to Biomass_core's Line 661-662 ",
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

