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
prepCohortData <- FALSE # Preamble. If already ran (i.e. objs cohortData2011 and cohortData2001 
                        # exist in inputs folder) this should NOT be run i.e. FALSE)

if (updateCRAN)
  update.packages(checkBuilt = TRUE, ask = FALSE)

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
    ".useCache" = FALSE,
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
    "pixelGroupAgeClass" = 10,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50,
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover | ecoregionGroup)))
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
  "ecoregionRst" = ecoRegionRAS,
  "t1" = t1
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
  originalOutputPath  <- Paths$outputPath
  Paths$outputPath <- Paths$inputPath
  outputsPreamble <- data.frame(objectName = c("cohortData", "pixelGroupMap"),
                                saveTime = 2001,
                                file = c("cohortData2001_fireSense", "pixelGroupMap2001_fireSense"))
  
  # 1. Run borealBiomassDataPrep ALONE and save: cohortData + pixelGroupMap: will be used 
  # in fireSense_SizeFit and fireSense_SpreadFit (later on, will be also used in Ignition and Escape fits)
  # 271 unique using ecoregion
  # 973 unique using ecodistrict
  
  biomassMaps2001 <- Cache(simInitAndSpades, 
                           times = list(start = 1, end = 1),
                           params = parameters,
                           modules = list("Biomass_borealDataPrep"),
                           objects = objects,
                           paths = getPaths(),
                           loadOrder = "Biomass_corealDataPrep",
                           outputs = outputsPreamble,
                           userTags = c("objective:preambleBiomassDataPrep", "time:year2001"))
  
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
                                file = c("cohortData2011_fireSense", "pixelGroupMap2011_fireSense"))
  
  objectsPre <- objects
  objectsPre$speciesLayers <- speciesLayers2011
  
  # and pass as object to a second call of Biomass_borealDataPrep. Save cohortData + pixelGroupMap.
  biomassMaps2011 <- Cache(simInitAndSpades,
                           times = list(start = 2011, end = 2011),
                           params = parameters,
                           modules = list("Biomass_borealDataPrep"),
                           objects = objectsPre,
                           paths = getPaths(),
                           loadOrder = "Biomass_borealDataPrep",
                           outputs = outputsPreamble,
                           userTags = c("objective:preambleBiomassDataPrep", "time:year2011"))
  
  Paths$outputPath <- originalOutputPath # return original path
  
}

# Now I have to generate the data to fit the Size module -- This was modified from the fireSense_Tutorial
# to accommodate real data

# Calculate mean landcover and weather conditions around the locations of escaped fires
# fireLocations <- getFirePoints_NFDB(url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip", 
#                                     studyArea = studyArea, rasterToMatch = rasterToMatch, 
#                                     NFDB_pointPath = file.path(Paths$inputPath, "NFDB_point"))
# Subset fires from 1991 - 2017
# fireLocations1991_2017 <- 

# Convert fire size from ha to m2 by multiplying by 10000, then divide by pi and take the Sqrt 
# of the result to go from points to radiuss
# fireLocations$radius <- sqrt(10000*fireLocations$SIZE_HA/pi)
# 
# 
# buf_around_loc_escaped <- buffer(fireLocations, byid = TRUE, dissolve = FALSE, 
#                                  width = fireLocations$radius) # THIS width is coming from the total area burned.

# Or we use the real polygons
# fireLocations <- reproducible::prepInputs(url = "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_1986_to_2018_20191129.zip",
#                                           studyArea = studyArea, rasterToMatch = rasterToMatch,
#                                           destinationPath = Paths$inputPath, 
#                                           userTags = c("typeOfFile:fireComposite", "objectName:fireLocations", "goal:fireSenseFit"))
#                                           # ABOVE FAILING BECAUSE OF SOME INTERSECTION PROBLEM... WILL TRY YEARLY

# So, with the above failing, I manually downloaded the fire data for each year. Deviding into pre 2006 and post 2006 because the 2006 layer has problems
# fireLocations1991_2005 <- Cache(getFirePolygons, years = 1991:2005, studyArea = studyArea, 
#                                  pathInputs = Paths$inputPath, userTags = c("years:1991_2005"))
# 
# fireLocations2007_2017 <- Cache(getFirePolygons, years = 2007:2017, studyArea = studyArea, 
#                                  pathInputs = Paths$inputPath, userTags = c("years:1991_2005"))
# 
# fireLocations <- c(fireLocations1991_2005, fireLocations2007_2017)
 
fireLocations <- Cache(getFirePolygons, years = 1991:2017, studyArea = studyArea, 
                                pathInputs = Paths$inputPath, userTags = c("years:1991_2017"))

# After getting the fire, I should get the weather (MDC)
# I downloaded the data manually using climateNA and placed in the /inputs folder
MDC <- calculateMDC(pathInputs = file.path(Paths$inputPath, "NWT_3ArcMinuteM"), 
                    years = c(1991:2017), doughtMonths = 4:9, rasterToMatch = rasterToMatch)
# "/mnt/data/Micheletti/NWT/inputs/NWT_3ArcMinuteM"

# Extract the fire polygons from the landscape
# Here I am going to do conifers and deciduous and the two landscape types



landtypeTypeOneBufMn <- extract(landTypeOne, buf_around_loc_escaped) %>% 
  lapply(mean) %>% 
  unlist

landtypeTypeTwoBufMn <- extract(landTypeTwo, buf_around_loc_escaped) %>% 
  lapply(mean) %>% 
  unlist

weatherBufMn <- extract(weather, buf_around_loc_escaped) %>%
  lapply(mean) %>%
  unlist


dataFireSense_SizeFit <- data.table(
  landtype_1_pp = landtypeTypeOneBufMn,
  landtype_2_pp = landtypeTypeTwoBufMn,
  weather = weatherBufMn
)

b1_l <- -.03
b2_l <- 4
b3_l <- -2

lambda <- with(dataFireSense_SizeFit, exp(weather * b1_l + landtype_1_pp * b2_l + landtype_2_pp * b3_l))

b1_t <- .02
b2_t <- 4
b3_t <- 2

theta <- with(dataFireSense_SizeFit, exp(weather * b1_t + landtype_1_pp * b2_t + landtype_2_pp * b3_t))

fireSize <- round(rtappareto(n = length(lambda), lambda = lambda, theta = theta, a = 1))

dataFireSense_SizeFit[, fire_size := fireSize]
# dataFireSense_SizeFit <- mutate(dataFireSense_SizeFit, fire_size = fireSize)

# Create the fire attribute dataset that describes the starting locations 
# and the size of the fires to be spread. This is needed to fit the statistical model of spread probabilities
fireAttributesFireSense_SpreadFit <- SpatialPointsDataFrame(fireLocations[as.logical(escaped)], data = data.frame(size = fireSize))



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
